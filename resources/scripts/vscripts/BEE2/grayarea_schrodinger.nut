if (!("ga_schrd_scopes" in getroottable())) {
    ::ga_schrd_scopes <- {};  // Global table to store all entity scopes by self_key
    ::ga_schrd_inputs <- {};  // Global table to store all input $ind numbers by self_key
    ::GASchrd <- function(entity) {
        local inst = split(entity.GetName(), "-")[0];
        printl("GASchrd called: " + inst);
        if (inst in ::ga_schrd_scopes) {
            local s = ::ga_schrd_scopes[inst].selfScope();
            if (s) {
                printl("Returning scope for " + s.self.GetName());
                return s;
            } else {
                printl("No selfScope found for entity: " + entity.GetName());
            }
        } else {
            printl("No local scope found for entity: " + entity.GetName());
            return null;
        }
    }
}

local name = self.GetName();
local re = regexp(@"^([^\-\s]+)-cube_addon_catcher_([^_]+)_([^_]+)_([^_]+)_([^_]+)_([^_]+)_([^_]+)_([^_]+)_([^_&]+)(&.+)?$");
local match = re.capture(name);

if (match != null) {
    inst_fixup <- name.slice(match[1].begin, match[1].end);
    inst_name <- "cube_addon_catcher";
    group <- name.slice(match[2].begin, match[2].end);
    ind <- name.slice(match[3].begin, match[3].end);
    next <- name.slice(match[4].begin, match[4].end);
    connectioncount <- name.slice(match[5].begin, match[5].end).tointeger();
    input_on_delay <- name.slice(match[6].begin, match[6].end).tofloat();
    input_off_delay <- name.slice(match[7].begin, match[7].end).tofloat();
    output_on_delay <- name.slice(match[8].begin, match[8].end).tofloat();
    output_off_delay <- name.slice(match[9].begin, match[9].end).tofloat();
    id <- match[10] != null ? name.slice(match[10].begin, match[10].end) : "";
    printl(
        "Parsed: inst_fixup=" + inst_fixup + ", inst_name=" + inst_name + ", group=" + group + ", ind=" + ind + ", next=" + next +
        ", connectioncount=" + connectioncount + ", input_on_delay=" + input_on_delay +
        ", input_off_delay=" + input_off_delay + ", output_on_delay=" + output_on_delay +
        ", output_off_delay=" + output_off_delay + ", id=" + id
    );
} else {
    // Debugging: Split the name to show components
    local parts = split(name, "-_");
    local debugMsg = "Invalid entity name format: " + name + "\nComponents: ";
    foreach (i, part in parts) {
        debugMsg += part + (i < parts.len() - 1 ? ", " : "");
    }
    throw debugMsg;
}
printl(group + "/" + ind + "/" + next + "/" + connectioncount + "/" + id);
self_key <- group + "_" + ind;
next_key <- group + "_" + next;
box_target <- inst_fixup + "-box" + id;
box <- Entities.FindByName(null, box_target);
printl("box_target: " + box_target + " " + box);
// player <- Entities.FindByClassname(null, "player");
// DoEntFire("@grayarea_schr_camera_" + group + "_" + ind, "SetParent", inst_fixup + "-box", 0, null, null);
// self_camera_target <- "@grayarea_schr_camera_" + group + "_" + ind;
// next_camera_target <- "@grayarea_schr_camera_" + group + "_" + next;
// camera <- null;
camera <- null;
monitor <- null;
blocker <- null;
glow <- null;
emitter <- null;
// self_controller_name <- "@grayarea_schr_controller_" + group + "_" + ind;
// self_controller <- null;
// next_controller_name <- "@grayarea_schr_controller_" + group + "_" + next;
// next_controller <- null;
// self.SetSize(Vector(-0.05,-0.05,-0.05), Vector(0.05,0.05,0.05));
EntFireByHandle(self, "RunScriptCode", "self.SetSize(Vector(-0.05,-0.05,-0.05), Vector(0.05,0.05,0.05))", 0, null, null);
// printl("@grayarea_schr_camera_" + group + "_" + next + " = " + camera);
// box_angles <- null;
// player_angles <- null;
monitor_alpha <- 0;
next_camera <- null;
guide <- null;
held <- false;
input <- false;
input_thread <- 0;
input_fired <- false;
output <- false;
output_thread <- 0;
output_fired <- false;
input_count <- 0;
move_thread <- 0;
logic_and <- true; // determines whether all inputs or any input must be activated.
// blocker_enabled <- true;

function selfScope() {
    if(self_key in ::ga_schrd_scopes) {
        return ::ga_schrd_scopes[self_key];
    }
}

function nextScope() {
    if(next_key in ::ga_schrd_scopes) {
        return ::ga_schrd_scopes[next_key];
    }
}

function spawn() {
    printl(self_key + id + "_spawn");
    if(box == null) {
        printl("Box not found: " + box_target);
        return;
    }
    EntFireByHandle(self, "SetParent", box_target, 0, null, null);
}

function droppered() {
    printl(self_key + id + "_droppered");
    local emitter_target = inst_fixup + "-cube_addon_emitter";
    emitter = entLib.FindByName(emitter_target);
    camera = entLib.FindByName(inst_fixup + "-cube_addon_camera");
    monitor = entLib.FindByName(inst_fixup + "-cube_addon_monitor");
    blocker = entLib.FindByName(inst_fixup + "-cube_addon_blocker" + id);
    glow = entLib.FindByName(inst_fixup + "-cube_addon_glow");
    if(id == "") {
        // We're an original/non-replacement cube.
        printl("Emitter: " + emitter);
        startMove();
    } else {
        // We're a replacement cube. Take over the original emitter and input state.
        emitter.Kill();
        local s = selfScope();
        emitter = s.emitter;
        s.emitter = null;
        printl("Taking over emitter: " + emitter + " and killing " + emitter_target);
        input_count = s.input_count;
        if(s.input) {
            input_count -= 1;
            inputOn();
        } else {
            startMove();
        }
    }
    local scope = self.GetScriptScope();
    if(ind != "$ind" && next != "$next" && !(self_key in ga_schrd_scopes)) {
        // Adding this key for the first time. Update ga_schrd_inputs.
        if(next_key in ga_schrd_inputs) {
            ga_schrd_inputs[next_key].push(ind);
        } else {
            ga_schrd_inputs[next_key] <- [ind];
        }
    }
    ga_schrd_scopes[self_key] <- scope;
    ga_schrd_scopes[inst_fixup] <- scope;
    guide = entLib.FindByName(inst_fixup + "-cube_addon_ref_cube_las");
    if(guide) {
        local sprite_name = inst_fixup + "-cube_addon_ref_cube_las_spr";
        local sprite = entLib.FindByName(sprite_name);
        local target_name = inst_fixup + "-cube_addon_ref_cube_las_targ";
        local target = entLib.FindByName(target_name);
        if(sprite) {
            EntFireByHandle(sprite, "SetLocalOrigin", "20 0 0", 0.01, null, null);
        } else {
            printl("Sprite not found: " + sprite_name);
        }
        if(target) {
            EntFireByHandle(target, "SetLocalOrigin", "9999999 0 0", 0.01, null, null);
        } else {
            printl("Target not found: " + target_name);
        }
    }
}

function fizzle() {
    printl(self_key + id + "_fizzle");
}

/**
 * We use a function to simulate parenting of the emitter to the box so that we can
 * update it as frequently as possible when the laser is on.
 * This also allows the emitter to survive fizzling of the cube so that it can be
 * reused. This is necessary to retain connections to non-cube entities.
 */
function startMove() {
    move_thread += 1;
    move(move_thread);
}

function move(thread) {
    if(!emitter) {
        // Waiting in dropper.
        // printl("Emitter not found, waiting in dropper.");
        moveAfter(thread, 1);
        return;
    }
    // printl("Emitter: " + emitter.GetName() + ", box: " + box.GetName());
    local bo = box.GetOrigin();
    local ba = box.GetAngles();

    // Get the box's velocity
    local velocity = box.GetVelocity();
    // local angVelocity = box.GetAngularVelocity();
    local predictedPos = Vector(
        bo.x + (velocity.x * 0.03),
        bo.y + (velocity.y * 0.03),
        bo.z + (velocity.z * 0.03)
    );
    // local predictedAng = Vector(
    //     ba.x + (angVelocity.x * 0.03),
    //     ba.y + (angVelocity.y * 0.03),
    //     ba.z + (angVelocity.z * 0.03)
    // );
    local forward = box.GetForwardVector();
    local newOrigin = Vector(
        predictedPos.x + (forward.x * 28),
        predictedPos.y + (forward.y * 28),
        predictedPos.z + (forward.z * 28)
    );
    emitter.SetOrigin(newOrigin);
    emitter.SetAngles(ba.x, ba.y, ba.z);
    moveAfter(thread, input ? 0.015 : 1); // 66Hz if input is on, otherwise 1 second
}

function moveAfter(thread, delay) {
    // Only continue the thread if the thread number has not changed due to a new call to startMove.
    if(thread == move_thread)
        EntFireByHandle(self, "RunScriptCode", "move(" + thread + ")", delay, self, self);
}

function pickup() {
    printl(self_key + id + "_pickup");
    guideOn();
    local ns = nextScope();
    if(ns) {
        next_camera = ns.activateCamera();
        if(next_camera) {
            printl("Setting monitor target to " + next_camera.GetName());
            EntFireByHandle(monitor, "SetCamera", next_camera.GetName(), 0, null, null);
        } else {
            printl("No next camera found");
        }
        ns.guideOn();
    }
    held = true;
    _check();
}

function _check() {
    if(!held && (!next_camera || monitor_alpha == 0))
        return;
    if(next_camera) {
        // printl("held " + held + ", monitor_alpha " + monitor_alpha);
        if(held) {
            monitor_alpha += 50;
            if(monitor_alpha > 255) {
                monitor_alpha = 255;
            }
        } else {
            monitor_alpha -= 25;
            if(monitor_alpha < 0) {
                monitor_alpha = 0;
            }
        }
        // printl("monitor_alpha: " + monitor_alpha);
        EntFireByHandle(monitor, "AddOutput", "renderamt " + monitor_alpha, 0, null, null);
    }
    EntFireByHandle(self, "CallScriptFunction", "_check", 0.1, self, self);
}

function incrementConnectionCount() {
    connectioncount += 1;
    printl(self_key + id + "_incrementConnectionCount: " + connectioncount);
}

function activateCamera() {
    printl(self_key + id + "_activateCamera");
    if(camera) {
        EntFireByHandle(camera, "ChangeFOV", "40 0.4", 0, null, null);
        EntFireByHandle(camera, "SetOnAndTurnOthersOff", "", 0, null, null);
    } else {
        EntFireByHandle(monitor, "AddOutput", "renderamt 0", 0, null, null);
    }
    return camera;
}

function setCameraFOV(fov, time) {
    printl(self_key + id + "_setCameraFOV " + fov + " " + time);
    if(camera)
        EntFireByHandle(camera, "ChangeFOV", fov + " " + time, 0, null, null);
}

function drop() {
    printl(self_key + id + "_drop");
    held = false;
    guideOff();
    local ns = nextScope();
    if(ns) {
        ns.setCameraFOV(70, 1);
        ns.guideOff();
    }
}

function guideOn() {
    if(guide) {
        EntFireByHandle(guide, "FireUser2", "", 0, null, null);
    }
}

function guideOff() {
    if(guide)
        EntFireByHandle(guide, "FireUser1", "", 0, null, null);
}

function totalInputs() {
    if(self_key in ga_schrd_inputs) {
        return ga_schrd_inputs[self_key].len() + connectioncount;
    } else {
        return connectioncount;
    }
}

function inputOn() {
    local total = totalInputs();
    input_count += 1;
    input_count = input_count > total ? total : input_count;
    printl(self_key + id + "_inputOn " + input_count + "/" + total);
    if(!input && (logic_and ? input_count == total : input_count > 0)) {
        input = true;
        fireInput(input_on_delay / 1000);
    }
}

function inputOff() {
    local total = totalInputs();
    input_count -= 1;
    input_count = input_count < 0 ? 0 : input_count;
    printl(self_key + id + "_inputOff" + " " + input_count + "/" + total);
    if(input && (logic_and ? input_count < total : input_count == 0)) {
        input = false;
        fireInput(input_off_delay / 1000);
    }
}

function fireInput(delay, thread = null) {
    if(thread == null) {
        input_thread += 1;
        thread = input_thread;
    }
    printl(self_key + id + "_fireInput " + input + ", delay: " + delay + ", thread: " + thread);
    if(delay > 0) {
        EntFireByHandle(self, "RunScriptCode", "fireInput(0, " + thread + ")", delay, self, self);
    } else if(thread == input_thread) {
        if(input) {
            if(!input_fired) {
                input_fired = true;
                startMove();
                updateBlocker();
                EntFireByHandle(emitter, "TurnOn", "", 0, null, null);
                EntFireByHandle(glow, "Start", "", 0, null, null);
            }
        } else if(input_fired) {
            input_fired = false;
            updateBlocker();
            EntFireByHandle(emitter, "TurnOff", "", 0, null, null);
            EntFireByHandle(glow, "Stop", "", 0, null, null);
        }
    }
}

function outputOn() {
    printl(self_key + id + "_outputOn");
    output = true;
    fireOutput(output_on_delay / 1000);
}

function outputOff() {
    printl(self_key + id + "_outputOff");
    output = false;
    fireOutput(output_off_delay / 1000);
}

function fireOutput(delay, thread = null) {
    if(thread == null) {
        output_thread += 1;
        thread = output_thread;
    }
    printl(self_key + id + "_fireOutput " + output + ", delay: " + delay + ", thread: " + thread);
    if(delay > 0) {
        EntFireByHandle(self, "RunScriptCode", "fireOutput(0, " + thread + ")", delay, self, self);
    } else if(thread == output_thread) {
        if(output) {
            if(!output_fired) {
                output_fired = true;
                EntFireByHandle(emitter, "FireUser1", "", 0, null, null);
                local ns = nextScope();
                if(ns)
                    ns.inputOn();
                updateBlocker();
            }
        } else if(output_fired) {
            output_fired = false;
            EntFireByHandle(emitter, "FireUser2", "", 0, null, null);
            local ns = nextScope();
            if(ns)
                ns.inputOff();
            updateBlocker();
        }
    }
}

// funtion updateBlocker() {
//     local enabled = input && output;
//     if(enabled) {
//         if(!blocker_enabled) {
//             blocker_enabled = true;
//            printl("EntFireByHandle call, target: " + (blocker != null ? blocker.GetName() : "null") + ", action: Enable");
//             EntFireByHandle(blocker, "Enable", "", 0, null, null);
//             printl(self_key + id + "_blocker enabled");
//         }
//     } else {
//         if(blocker_enabled) {
//             blocker_enabled = false;
//             printl("EntFireByHandle call, target: " + (blocker != null ? blocker.GetName() : "null") + ", action: Disable");
//             EntFireByHandle(blocker, "Disable", "", 0, null, null);
//             printl(self_key + id + "_blocker disabled");
//         }
//     }
// }
function updateBlocker() {
    if(input) {
        printl(self_key + id + "_blocker disabled");
        // printl("EntFireByHandle call, target: " + (blocker != null ? blocker.GetName() : "null") + ", action: Disable");
        // EntFireByHandle(blocker, "Disable", "", 0, null, null);
        printl("Blocker: " + blocker);
        blocker.Disable();
    } else {
        printl(self_key + id + "_blocker enabled");
        printl("Blocker: " + blocker);
        blocker.Enable();
        // printl("EntFireByHandle call, target: " + (blocker != null ? blocker.GetName() : "null") + ", action: Enable");
        // EntFireByHandle(blocker, "Enable", "", 0, null, null);
    }
}
