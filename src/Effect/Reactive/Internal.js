"use strict";

const debugMode =
  localStorage.getItem("Effect.Reactive.Internal.debugMode") === "true";

const NETWORK_OFFLINE = debugMode ? "OFFLINE" : 0;
const NETWORK_STANDBY = debugMode ? "STANDBY" : 1;
const NETWORK_SCHEDULED = debugMode ? "SCHEDULED" : 2;
const NETWORK_EVALUATING = debugMode ? "EVALUATING" : 3;
const NETWORK_EMITTING = debugMode ? "EMITTING" : 4;
const NETWORK_SUSPENDED = debugMode ? "SUSPENDED" : 5;

const debug = debugMode ? console.debug : () => {};
const log = debugMode ? console.log : () => {};
const warn = debugMode ? console.warn : () => {};

function TimeoutScheduler() {
  return {
    schedule: function timeoutSchedulerSchedule(task) {
      setTimeout(task, 0);
    },
  };
}

function traverse(iterator, f) {
  let iteration = iterator.next();
  while (!iteration.done) {
    f(iteration.value);
    iteration = iterator.next();
  }
}

function Node(id, fire) {
  const self = new EventTarget();

  self.id = id;
  self.outputs = new Map();
  self.inputs = new Map();
  self.parents = new Map();
  self.children = new Map();

  self.traverseChildren = function Node_traverseChildren(f) {
    traverse(self.children.values(), f);
  };

  self.traverseParents = function Node_traverseParents(f) {
    traverse(self.parents.values(), f);
  };

  function isConnected() {
    return self.inputs.size > 0 && self.outputs.size > 0;
  }

  function updateIO(f) {
    const connectedBefore = isConnected();
    f();
    const connectedAfter = isConnected();
    if (!connectedBefore && connectedAfter) {
      self.dispatchEvent(new Event("connected"));
    } else if (connectedBefore && !connectedAfter) {
      self.dispatchEvent(new Event("disconnected"));
    }
  }

  self.addInput = function Node_addInput(input) {
    updateIO(function () {
      const count = self.inputs.get(input) || 0;
      self.inputs.set(input, count + 1);
      self.traverseChildren((c) => c.addInput(input));
    });
  };

  self.addOutput = function Node_addOutput(output) {
    updateIO(function () {
      const count = self.outputs.get(output) || 0;
      self.outputs.set(output, count + 1);
      self.traverseParents((p) => p.addInput(output));
    });
  };

  self.removeInput = function Node_removeInput(id) {
    updateIO(function () {
      const count = self.inputs.get(id);
      if (count) {
        const newCount = count - 1;
        if (newCount) {
          self.inputs.set(id, count + 1);
        } else {
          self.inputs.delete(id);
        }
        self.traverseChildren((c) => c.removeInput(id));
      }
    });
  };

  self.removeOutput = function Node_removeOutput(id) {
    updateIO(function () {
      const count = self.outputs.get(id);
      if (count) {
        const newCount = count - 1;
        if (newCount) {
          self.outputs.set(id, count + 1);
        } else {
          self.outputs.delete(id);
        }
        self.traverseParents((p) => p.removeOutput(id));
      }
    });
  };

  self.addParent = function Node_addParent(parent) {
    self.parents.set(parent.id, parent);
    parent.addChild(self);
  };

  self.removeParent = function Node_removeParent(parent) {
    self.parents.delete(parent.id);
    parent.removeChild(self);
  };

  self.addChild = function Node_addChild(child) {
    self.children.set(child.id, child);
    traverse(child.outputs.keys(), self.addOutput);
    traverse(self.inputs.keys(), child.addInput);
  };

  self.removeChild = function Node_removeChild(child) {
    self.children.delete(child.id);
    traverse(child.outputs.keys(), self.removeOutput);
    traverse(self.inputs.keys(), child.removeInput);
  };

  self.fire = function Node_fire(value) {
    if (self.outputs.size > 0 && self.inputs.size > 0) {
      debug("Node.fire", id, value);
      fire(value, self);
    }
  };

  return self;
}

function InputNode(id, fire) {
  const self = Node(id, fire);
  self.addInput(self.id);
  return self;
}

function OutputNode(id, fire) {
  const self = Node(id, fire);
  self.addOutput(self.id);
  return self;
}

function Network(scheduler) {
  const self = new EventTarget();
  let nextNodeId = 0;
  const raisedInputs = new Map();
  const raisedOutputs = new Map();

  self.status = NETWORK_OFFLINE;

  function evaluate() {
    debug("Network.evaluate");
    if (self.status === NETWORK_OFFLINE) return;
    self.status = NETWORK_EVALUATING;
    traverse(raisedInputs.values(), function ({ value, input }) {
      input.traverseChildren((c) => c.fire(value));
    });
    raisedInputs.clear();
    if (self.status === NETWORK_OFFLINE) return;
    self.status = NETWORK_EMITTING;
    traverse(raisedOutputs.values(), function ({ value, sink }) {
      sink(value);
    });
    raisedOutputs.clear();
    self.status = NETWORK_STANDBY;
  }

  function newNode(makeNode) {
    debug("Network.newNode");
    const id = nextNodeId++;
    const node = makeNode(id);
    const superFire = node.fire;
    node.fire = function Network_newNode_fire(value) {
      switch (self.status) {
        case NETWORK_OFFLINE:
        case NETWORK_SUSPENDED:
          break;
        default:
          superFire(value);
          break;
      }
    };
    return node;
  }

  self.newInput = function Network_newInput() {
    debug("Network.newInput");
    return newNode(function Network_newInput_makeNode(id) {
      return InputNode(id, function Network_newInput_fire(value, input) {
        switch (self.status) {
          case NETWORK_STANDBY:
            raisedInputs.set(id, { value, input });
            scheduler.schedule(evaluate);
            break;
          case NETWORK_SCHEDULED:
            raisedInputs.set(id, { value, input });
            break;
          case NETWORK_EVALUATING:
          case NETWORK_EMITTING:
            scheduler.schedule(() => input.fire(value));
            break;
        }
      });
    });
  };

  self.newOutput = function Network_newOutput(sink) {
    debug("Network.newOutput");
    return newNode(function Network_newOutput_makeNode(id) {
      return OutputNode(id, function Network_newOutput_fire(value, output) {
        switch (self.status) {
          case NETWORK_EVALUATING:
            raisedOutputs.set(id, { value, sink });
            break;
        }
      });
    });
  };

  self.actuate = function Network_actuate() {
    if (self.status === NETWORK_OFFLINE) {
      self.status = NETWORK_STANDBY;
      self.dispatchEvent(new Event("actuated"));
    }
  };

  self.deactivate = function Network_deactivate() {
    if (self.status !== NETWORK_OFFLINE) {
      self.status = NETWORK_OFFLINE;
      self.dispatchEvent(new Event("deactivated"));
      raisedInputs.clear();
      raisedOutputs.clear();
    }
  };

  self.suspend = function Network_suspend() {
    switch (self.status) {
      case NETWORK_OFFLINE:
      case NETWORK_SUSPENDED:
        break;
      case NETWORK_STANDBY:
      case NETWORK_SCHEDULED:
        self.status = NETWORK_SUSPENDED;
        self.dispatchEvent(new Event("suspended"));
        raisedInputs.clear();
        raisedOutputs.clear();
        break;
      default:
        scheduler.schedule(self.suspend);
        break;
    }
  };

  self.resume = function Network_resume() {
    if (self.status === NETWORK_SUSPENDED) {
      self.status = NETWORK_STANDBY;
      self.dispatchEvent(new Event("resumed"));
    }
  };

  return self;
}
