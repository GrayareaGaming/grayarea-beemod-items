if (!("ga_schrd_scopes" in getroottable())) {
    ::ga_schrd_scopes <- {};  // Global table to store all entity scopes by self_key
    ::ga_schrd_inputs <- {};  // Global table to store all input $ind numbers by self_key
    ::ga_schrd_emitters <- {}; // Global table to store all emitter entities by self_key
    ::GASchrd <- function(entity) {
        local inst = split(entity.GetName(), "-")[0];
        if (inst in ::ga_schrd_scopes) {
            local s = ::ga_schrd_scopes[inst].selfScope();
            if (s) {
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
// set inst_fixup to the name up to the first hyphen
inst_fixup <- split(name, "-_")[0];
local i = name.find("&");
id <- i == null ? "" : name.slice(i);

camera <- null;
monitor <- null;
blocker <- null;
glow <- null;
emitter <- null;
EntFireByHandle(self, "RunScriptCode", "self.SetSize(Vector(-0.05,-0.05,-0.05), Vector(0.05,0.05,0.05))", 0, null, null);
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
input_box <- null;
trace <- function(msg) { };

function spawn(options = {}) {
    // Fixups are all passed via the blocker OnUser1 output and get assigned to the script scope.
    local s = self.GetScriptScope();
    foreach(key, val in options)
        s[key] <- val;
    start_enabled = !!start_enabled;
    start_active = !!start_active;
    start_reversed = !!start_reversed;
    trace_log = !!trace_log;
    if(trace_log)
        trace = printl;
    self_key <- group + "_" + ind;
    next_key <- group + "_" + next;
    if(trace_log) {
        trace(self_key + id + "_spawn, options:");
        __DumpScope(1, options);
    }
    box_target <- inst_fixup + "-box" + id;
    box <- Entities.FindByName(null, box_target);
    if(box == null)
        throw "Box not found: " + box_target;
    trace("box_target: " + box_target + " " + box);
    logic_and <- !start_active; // determines whether all inputs or any input must be activated.
    EntFireByHandle(self, "SetParent", box_target, 0, null, null);
}

function droppered() {
    trace(self_key + id + "_droppered");
    local emitter_target = inst_fixup + "-cube_addon_emitter";
    emitter = entLib.FindByName(emitter_target);
    blocker = entLib.FindByName(inst_fixup + "-cube_addon_blocker" + id);
    glow = entLib.FindByName(inst_fixup + "-cube_addon_glow");
    if(self_key in ga_schrd_emitters) {
        // We're a replacement cube. Take over the original emitter and input state.
        // trace("Emitter target: " + emitter_target + ", emitter: " + (emitter ? emitter.IsValid() ? "valid" : "invalid" : "null"));
        // emitter.Kill();
        DoEntFire(emitter_target, "Kill", "", 1, null, null);
        emitter = ga_schrd_emitters[self_key];
        local s = selfScope();
        s.emitter = null;
        trace("Taking over emitter: " + emitter + " and killing " + emitter_target);
        input_count = s.input_count;
        if(s.input) {
            input_count -= 1;
            inputOn();
        } else {
            startMove();
        }
    } else {
        // We're an original/non-replacement cube.
        trace("Emitter: " + emitter);
        startMove();
        ga_schrd_emitters[self_key] <- emitter;
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
    if(!start_reversed) {
        local ns = nextScope();
        if(ns && box && box.IsValid())
            ns.setInputBox(box);
    }
    if(self_key in ga_schrd_inputs) {
        local inputs = ga_schrd_inputs[self_key];
        for(local i = 0; i < inputs.len(); i++) {
            local prev_key = group + "_" + inputs[i];
            if(prev_key in ga_schrd_scopes) {
                local ps = ga_schrd_scopes[prev_key];
                if(ps) {
                    if("start_reversed" in ps && !ps.start_reversed && "box" in ps) {
                        local box = ps.box;
                        if(box && box.IsValid())
                            setInputBox(box);
                    }
                }
            }
        }
    }
    guide = entLib.FindByName(inst_fixup + "-cube_addon_ref_cube_las");
    if(guide) {
        local sprite_name = inst_fixup + "-cube_addon_ref_cube_las_spr" + id;
        local sprite = entLib.FindByName(sprite_name);
        local target_name = inst_fixup + "-cube_addon_ref_cube_las_targ" + id;
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
    if(start_enabled) {
        trace(self_key + id + "_start enabled");
        input = true;
        fireInput(0.01);
    }
}

function fizzle() {
    trace(self_key + id + "_fizzle");
    if(!start_reversed) {
        local ns = nextScope();
        if(ns) {
            trace("Fizzling, calling nextScope.kill()");
            ns.kill();
        }
    }
}

function kill() {
    trace(self_key + id + "_kill");
    if(box)
        EntFireByHandle(box, "dissolve", "", 0, null, null);
}

function setInputBox(box) {
    trace(self_key + id + "_setInputBox: " + box.GetName());
    input_box <- box;
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
    if(!emitter || !emitter.IsValid() || !box || !box.IsValid())
        return;
    // trace(self_key + id + "_move, input_box: " + (input_box ? input_box.IsValid() ? "valid" : "invalid" : "null"));
    if(input_box && !input_box.IsValid()) {
        kill();
        return;
    }
    // trace("Emitter: " + emitter.GetName() + ", box: " + box.GetName());
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
    moveAfter(thread, input ? 0.015 : 0.5); // 66Hz if input is on, otherwise 2Hz.
}

function moveAfter(thread, delay) {
    // Only continue the thread if the thread number has not changed due to a new call to startMove.
    if(thread == move_thread)
        EntFireByHandle(self, "RunScriptCode", "move(" + thread + ")", delay, self, self);
}

function pickup() {
    trace(self_key + id + "_pickup");
    guideOn();
    local ns = nextScope();
    if(ns) {
        next_camera = ns.activateCamera();
        if(next_camera && next_camera.IsValid()) {
            trace("Setting monitor target to " + next_camera.GetName());
            monitor = entLib.FindByName(inst_fixup + "-cube_addon_monitor");
            EntFireByHandle(monitor, "SetCamera", next_camera.GetName(), 0, null, null);
        } else {
            trace("No next camera found");
        }
        ns.guideOn();
    }
    held = true;
    _check();
}

function _check() {
    local has_camera = next_camera && next_camera.IsValid();
    if(!held && (!has_camera || monitor_alpha == 0))
        return;
    if(has_camera) {
        // trace("held " + held + ", monitor_alpha " + monitor_alpha);
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
        // trace("monitor_alpha: " + monitor_alpha);
        EntFireByHandle(monitor, "AddOutput", "renderamt " + monitor_alpha, 0, null, null);
    }
    EntFireByHandle(self, "CallScriptFunction", "_check", 0.1, self, self);
}

function incrementConnectionCount() {
    connectioncount += 1;
    trace(self_key + id + "_incrementConnectionCount: " + connectioncount);
}

function activateCamera() {
    camera = entLib.FindByName(inst_fixup + "-cube_addon_camera");
    monitor = entLib.FindByName(inst_fixup + "-cube_addon_monitor");
    trace(self_key + id + "_activateCamera " + (camera ? camera.IsValid() ? "valid" : "invalid" : "null"));
    if(camera && camera.IsValid()) {
        EntFireByHandle(camera, "ChangeFOV", "40 0.4", 0, null, null);
        EntFireByHandle(camera, "SetOnAndTurnOthersOff", "", 0, null, null);
    } else {
        EntFireByHandle(monitor, "AddOutput", "renderamt 0", 0, null, null);
    }
    return camera;
}

function setCameraFOV(fov, time) {
    camera = entLib.FindByName(inst_fixup + "-cube_addon_camera");
    trace(self_key + id + "_setCameraFOV " + fov + " " + time + " " + (camera ? camera.IsValid() ? "valid" : "invalid" : "null"));
    if(camera && camera.IsValid())
        EntFireByHandle(camera, "ChangeFOV", fov + " " + time, 0, null, null);
}

function drop() {
    trace(self_key + id + "_drop");
    held = false;
    guideOff();
    local ns = nextScope();
    if(ns) {
        ns.setCameraFOV(70, 1);
        ns.guideOff();
    }
}

function guideOn() {
    guide = entLib.FindByName(inst_fixup + "-cube_addon_ref_cube_las");
    if(guide && guide.IsValid())
        EntFireByHandle(guide, "FireUser2", "", 0, null, null);
}

function guideOff() {
    guide = entLib.FindByName(inst_fixup + "-cube_addon_ref_cube_las");
    if(guide && guide.IsValid())
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
    trace(self_key + id + "_inputOn " + input_count + "/" + total + ", logic_and: " + logic_and + ", start_enabled: " + start_enabled + ", input: " + input);
    if(input == start_enabled && (logic_and ? input_count == total : input_count > 0)) {
        input = !start_enabled;
        fireInput(input_on_delay / 1000);
    }
}

function inputOff() {
    local total = totalInputs();
    input_count -= 1;
    input_count = input_count < 0 ? 0 : input_count;
    trace(self_key + id + "_inputOff" + " " + input_count + "/" + total);
    if(input == !start_enabled && (logic_and ? input_count < total : input_count == 0)) {
        input = start_enabled;
        fireInput(input_off_delay / 1000);
    }
}

function fireInput(delay, thread = null) {
    if(thread == null) {
        input_thread += 1;
        thread = input_thread;
    }
    trace(self_key + id + "_fireInput " + input + ", delay: " + delay + ", thread: " + thread);
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
    trace(self_key + id + "_outputOn");
    output = true;
    fireOutput(output_on_delay / 1000);
}

function outputOff() {
    trace(self_key + id + "_outputOff");
    output = false;
    fireOutput(output_off_delay / 1000);
}

function fireOutput(delay, thread = null) {
    if(thread == null) {
        output_thread += 1;
        thread = output_thread;
    }
    trace(self_key + id + "_fireOutput " + output + ", delay: " + delay + ", thread: " + thread);
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
//            trace("EntFireByHandle call, target: " + (blocker != null ? blocker.GetName() : "null") + ", action: Enable");
//             EntFireByHandle(blocker, "Enable", "", 0, null, null);
//             trace(self_key + id + "_blocker enabled");
//         }
//     } else {
//         if(blocker_enabled) {
//             blocker_enabled = false;
//             trace("EntFireByHandle call, target: " + (blocker != null ? blocker.GetName() : "null") + ", action: Disable");
//             EntFireByHandle(blocker, "Disable", "", 0, null, null);
//             trace(self_key + id + "_blocker disabled");
//         }
//     }
// }
function updateBlocker() {
    if(input) {
        trace(self_key + id + "_blocker disabled");
        // trace("EntFireByHandle call, target: " + (blocker != null ? blocker.GetName() : "null") + ", action: Disable");
        // EntFireByHandle(blocker, "Disable", "", 0, null, null);
        trace("Blocker: " + blocker);
        blocker.Disable();
    } else {
        trace(self_key + id + "_blocker enabled");
        trace("Blocker: " + blocker);
        blocker.Enable();
        // trace("EntFireByHandle call, target: " + (blocker != null ? blocker.GetName() : "null") + ", action: Enable");
        // EntFireByHandle(blocker, "Enable", "", 0, null, null);
    }
}

function selfScope() {
    if(self_key in ::ga_schrd_scopes)
        return ::ga_schrd_scopes[self_key];
}

function nextScope() {
    if(next_key in ::ga_schrd_scopes)
        return ::ga_schrd_scopes[next_key];
}
