"use strict";

const debugMode = false;

const NETWORK_OFFLINE = debugMode ? "OFFLINE" : 0;
const NETWORK_STANDBY = debugMode ? "STANDBY" : 1;
const NETWORK_SCHEDULED = debugMode ? "SCHEDULED" : 2;
const NETWORK_EVALUATING = debugMode ? "EVALUATING" : 3;
const NETWORK_EMITTING = debugMode ? "EMITTING" : 4;
const NETWORK_SUSPENDED = debugMode ? "SUSPENDED" : 5;

const debug = debugMode ? console.debug : () => {};
const log = debugMode ? console.log : () => {};
const warn = debugMode ? console.warn : () => {};

let nextSchedulerId = 0;

function AnimationFrameScheduler() {
  const id = nextSchedulerId++;
  let offset;
  function AnimationFrameScheduler_reset() {
    offset = performance.now();
  }
  AnimationFrameScheduler_reset();
  return {
    reset: AnimationFrameScheduler_reset,
    schedule: function AnimationFrameScheduler_schedule(task) {
      requestAnimationFrame(function AnimationFrameScheduler_run(timestamp) {
        task(timestamp - offset);
      });
    },
  };
}

function TimeoutScheduler() {
  const id = nextSchedulerId++;
  const marker = "timeout-schedule-" + id;
  const measurement = marker + "-measurement";
  function TimeoutScheduler_reset() {
    performance.mark(marker);
  }
  TimeoutScheduler_reset();
  return {
    reset: TimeoutScheduler_reset,
    schedule: function TimeoutScheduler_schedule(task) {
      setTimeout(function TimeoutScheduler_run() {
        performance.measure(measurement, marker);
        const [entry] = performance.getEntriesByName(measurement);
        performance.clearMeasures(measurement);
        task(entry.duration);
      }, 0);
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
  debug("Node", id);
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
    debug("Node.addInput", self.id, input);
    updateIO(function () {
      const count = self.inputs.get(input) || 0;
      self.inputs.set(input, count + 1);
      self.traverseChildren((c) => c.addInput(input));
    });
  };

  self.addOutput = function Node_addOutput(output) {
    debug("Node.addOutput", self.id, output);
    updateIO(function () {
      const count = self.outputs.get(output) || 0;
      self.outputs.set(output, count + 1);
      self.traverseParents((p) => p.addOutput(output));
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
    if (self.parents.has(parent.id)) return;
    debug("Node.addParent", self.id, parent.id);
    self.parents.set(parent.id, parent);
    parent.addChild(self);
  };

  self.removeParent = function Node_removeParent(parent) {
    self.parents.delete(parent.id);
    parent.removeChild(self);
  };

  self.addChild = function Node_addChild(child) {
    if (self.children.has(child.id)) return;
    debug("Node.addChild", self.id, child.id);
    self.children.set(child.id, child);
    child.addParent(self);
    traverse(child.outputs.keys(), self.addOutput);
    traverse(self.inputs.keys(), child.addInput);
  };

  self.removeChild = function Node_removeChild(child) {
    self.children.delete(child.id);
    traverse(child.outputs.keys(), self.removeOutput);
    traverse(self.inputs.keys(), child.removeInput);
  };

  self.fire = function Node_fire(value) {
    debug("Node.fire", id, value, self.outputs, self.inputs);
    if (self.outputs.size > 0 && self.inputs.size > 0) {
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

function LatchNode(id, initialValue, fire) {
  const self = Node(id, fire);
  let value = initialValue;
  self.write = function LatchNode_write(x) {
    debug("LatchNode.write", x);
    value = x;
  };
  self.read = function LatchNode_read() {
    debug("LatchNode.read", value);
    return value;
  };
  return self;
}

function MultiNode(id, inputs, outputs, fire) {
  const self = Node(id, fire);
  Object.values(inputs).forEach(self.addParent);
  Object.values(outputs).forEach(self.addChild);
  return self;
}

function Network(Just, Nothing, scheduler) {
  const self = new EventTarget();
  let nextNodeId = 0;
  const latchWrites = new Map();
  const nodeValues = new Map();
  const raisedNodes = new Map();
  const raisedMultiNodes = new Map();
  const raisedOutputs = new Map();

  self.status = NETWORK_OFFLINE;
  self.timestamp = Number.NEGATIVE_INFINITY;

  function flushMultiNodes() {
    const nodes = Array.from(raisedMultiNodes, function ([id, evalMulti]) {
      debug("Network.flushMultiNodes.process", id);
      return evalMulti;
    });
    raisedMultiNodes.clear();
    nodes.forEach(function Network_flushMultiNodes_process(evalMulti) {
      if (self.status === NETWORK_OFFLINE) return;
      evalMulti(self)();
    });
  }

  function drainNodes() {
    while (
      (raisedMultiNodes.size > 0 || raisedNodes.size > 0) &&
      self.status !== NETWORK_OFFLINE
    ) {
      if (raisedNodes.size > 0) {
        const nodes = Array.from(
          raisedNodes.values(),
          function Network_drainNodes_collect(item) {
            if (nodeValues.has(item.node.id)) {
              console.error(
                "Cycle detected in network. Node has been visited multiple times.",
                item.node
              );
              throw new Error(
                "Cycle detected in network. Node has been visited multiple times."
              );
            }
            nodeValues.set(item.node.id, item.value);
            return item;
          }
        );
        raisedNodes.clear();
        nodes.forEach(function Network_drainNodes_process({ value, node }) {
          if (self.status === NETWORK_OFFLINE) return;
          debug("Network.drainNodes.process", node.id, value);
          node.traverseChildren((c) => c.fire(value));
        });
      }
      if (self.status === NETWORK_OFFLINE) return;
      if (raisedNodes.size === 0 && raisedMultiNodes.size > 0) {
        flushMultiNodes();
      }
    }
  }

  function evaluate(timestamp) {
    debug("Network.evaluate");
    self.timestamp = timestamp;
    if (self.status === NETWORK_OFFLINE) return;
    self.status = NETWORK_EVALUATING;
    drainNodes();
    if (self.status === NETWORK_OFFLINE) return;
    nodeValues.clear();
    traverse(
      latchWrites.values(),
      function Network_evaluate_writeLatch({ value, latch }) {
        debug("Network.writeLatch", latch.id, value);
        latch.write(value);
      }
    );
    latchWrites.clear();
    self.status = NETWORK_EMITTING;
    traverse(raisedOutputs.entries(), function ([id, { value, sink }]) {
      debug("Network.emit", id, value);
      sink(value);
    });
    raisedOutputs.clear();
    debug("Network.evaluate - done");
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

  self.empty = new EventTarget();
  self.empty.id = nextNodeId++;
  self.empty.outputs = new Map();
  self.empty.inputs = new Map();
  self.empty.parents = new Map();
  self.empty.children = new Map();
  self.empty.traverseChildren = function Network_empty_traverseChildren() {};
  self.empty.traverseParents = function Network_empty_traverseParents() {};
  self.empty.addInput = function Network_empty_addInput() {};
  self.empty.addOutput = function Network_empty_addOutput() {};
  self.empty.removeInput = function Network_empty_removeInput() {};
  self.empty.removeOutput = function Network_empty_removeOutput() {};
  self.empty.addParent = function Network_empty_addParent() {};
  self.empty.removeParent = function Network_empty_removeParent() {};
  self.empty.addChild = function Network_empty_addChild() {};
  self.empty.removeChild = function Network_empty_removeChild() {};
  self.empty.fire = function Network_empty_fire() {};

  self.newBuffer = function Network_newBuffer() {
    debug("Network.newBuffer");
    return newNode(function Network_newBuffer_makeNode(id) {
      return Node(id, function Network_newBuffer_fire(value, node) {
        switch (self.status) {
          case NETWORK_EVALUATING:
            nodeValues.set(node.id, value);
            node.traverseChildren((c) => c.fire(value));
            break;
        }
      });
    });
  };

  self.newInput = function Network_newInput() {
    debug("Network.newInput");
    return newNode(function Network_newInput_makeNode(id) {
      return InputNode(id, function Network_newInput_fire(value, node) {
        switch (self.status) {
          case NETWORK_STANDBY:
            self.status = NETWORK_SCHEDULED;
            raisedNodes.set(id, { value, node });
            scheduler.schedule(evaluate);
            break;
          case NETWORK_SCHEDULED:
            raisedNodes.set(id, { value, node });
            break;
          case NETWORK_EVALUATING:
          case NETWORK_EMITTING:
            scheduler.schedule(() => node.fire(value));
            break;
        }
      });
    });
  };

  self.newOutput = function Network_newOutput(sink) {
    debug("Network.newOutput");
    return newNode(function Network_newOutput_makeNode(id) {
      return OutputNode(id, function Network_newOutput_fire(value) {
        switch (self.status) {
          case NETWORK_EVALUATING:
            raisedOutputs.set(id, { value, sink });
            break;
        }
      });
    });
  };

  self.newLatch = function Network_newLatch(initialValue) {
    debug("Network.newLatch");
    return newNode(function Network_newLatch_makeNode(id) {
      return LatchNode(
        id,
        initialValue,
        function Network_newLatch_fire(value, node) {
          switch (self.status) {
            case NETWORK_EVALUATING:
              raisedNodes.set(id, { value, node });
              latchWrites.set(id, { value, latch: node });
              break;
          }
        }
      );
    });
  };

  self.newExecute = function Network_newExecute(execute) {
    debug("Network.newExecute");
    return newNode(function Network_newExecute_makeNode(id) {
      return Node(id, function Network_newExecute_fire(inValue, node) {
        switch (self.status) {
          case NETWORK_EVALUATING:
            const outValue = execute(inValue)(self)();
            raisedNodes.set(id, { value: outValue, node });
            break;
        }
      });
    });
  };

  self.newProcess = function Network_newProcess(evalNode) {
    debug("Network.newProcess");
    return newNode(function Network_newProcess_makeNode(id) {
      return Node(id, function Network_newProcess_fire(inValue, node) {
        switch (self.status) {
          case NETWORK_EVALUATING:
            function Network_newProcess_raise(outValue) {
              return function Network_newProcess_raise_raff(network) {
                return function Network_newProcess_raise_eff() {
                  raisedNodes.set(id, { value: outValue, node });
                };
              };
            }
            evalNode(node)(inValue)(Network_newProcess_raise)(self)();
            break;
        }
      });
    });
  };

  self.newMulti = function Network_newMulti(inputs, outputs, evalMulti) {
    debug("Network.newMulti");
    const inputReads = {};
    const outputWrites = {};
    Object.entries(inputs).forEach(function ([k, inNode]) {
      function Network_newMulti_readInput_raff(network) {
        return function Network_newMulti_readInput_eff() {
          return self.readNode(inNode.id);
        };
      }
      inputReads[k] = Network_newMulti_readInput_raff;
    });
    Object.entries(outputs).forEach(function ([k, outNode]) {
      function Network_newMulti_writeOutput(value) {
        return function Network_newMulti_writeOutput_raff(network) {
          return function Network_newMulti_writeOutput_eff() {
            outNode.fire(value);
          };
        };
      }
      outputWrites[k] = Network_newMulti_writeOutput;
    });
    return newNode(function Network_newMulti_makeNode(id) {
      return MultiNode(
        id,
        inputs,
        outputs,
        function Network_newMulti_fire(_, node) {
          switch (self.status) {
            case NETWORK_EVALUATING:
              raisedMultiNodes.set(
                id,
                evalMulti(node)(inputReads)(outputWrites)
              );
              break;
          }
        }
      );
    });
  };

  self.actuate = function Network_actuate() {
    if (self.status === NETWORK_OFFLINE) {
      debug("Network.actuate");
      scheduler.reset();
      self.timestamp = 0;
      self.status = NETWORK_STANDBY;
      self.dispatchEvent(new Event("actuated"));
    }
  };

  self.deactivate = function Network_deactivate() {
    if (self.status !== NETWORK_OFFLINE) {
      self.status = NETWORK_OFFLINE;
      self.timestamp = Number.NEGATIVE_INFINITY;
      self.dispatchEvent(new Event("deactivated"));
      raisedNodes.clear();
      raisedMultiNodes.clear();
      latchWrites.clear();
      nodeValues.clear();
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
        raisedNodes.clear();
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

  self.readNode = function Network_readNode(id) {
    const value = nodeValues.has(id) ? Just(nodeValues.get(id)) : Nothing;
    debug("Network.readNode", value);
    return value;
  };

  return self;
}

// Node API

exports._addParent = function addParent(child, parent) {
  child.addParent(parent);
};

exports._removeParent = function removeParent(child, parent) {
  child.removeParent(parent);
};

exports._addChild = function addChild(parent, child) {
  parent.addChild(child);
};

exports._removeChild = function removeChild(parent, child) {
  parent.removeChild(child);
};

exports._fire = function fire(value, node) {
  node.fire(value);
};

exports._onConnected = function onConnected(node, eff) {
  let disconnectedListener;
  function connected() {
    const disconnected = eff();
    function wrapDisconnected() {
      disconnected();
      node.removeEventListener("disconnected", wrapDisconnected);
      disconnectedListener = null;
    }
    disconnectedListener = wrapDisconnected;
    node.addEventListener("disconnected", wrapDisconnected);
  }
  node.addEventListener("connected", connected);
  return function () {
    node.removeEventListener("connected", connected);
    if (disconnectedListener) {
      node.removeEventListener("disconnected", disconnectedListener);
    }
  };
};

// Network API

exports._withNetwork = function withNetwork(Just, Nothing, scheduler, eff) {
  const network = Network(Just, Nothing, scheduler);
  return eff(network)();
};

exports.newInput = function newInput(network) {
  return network.newInput;
};

exports.newBuffer = function newBuffer(network) {
  return network.newBuffer;
};

exports.emptyNode = function emptyNode(network) {
  return function () {
    return network.empty;
  };
};

exports.newOutput = function newOutput(eff) {
  return function newOutput_raff(network) {
    return function newOutput_eff() {
      return network.newOutput((a) => eff(a)());
    };
  };
};

exports.newLatch = function newLatch(initialValue) {
  return function newLatch_raff(network) {
    return function newLatch_eff() {
      return network.newLatch(initialValue);
    };
  };
};

exports.newProcess = function newProcess(evalNode) {
  return function newProcess_raff(network) {
    return function newProcess_eff() {
      return network.newProcess(evalNode);
    };
  };
};

exports.newExecute = function newExecute(execute) {
  return function newExecute_raff(network) {
    return function newExecute_eff() {
      return network.newExecute(execute);
    };
  };
};

exports._newMulti = function newMulti(inputs, outputs, evalMulti) {
  return function newMulti_raff(network) {
    return function newMulti_eff() {
      return network.newMulti(inputs, outputs, evalMulti);
    };
  };
};

exports.actuate = function actuate(network) {
  return network.actuate;
};

exports.deactivate = function deactivate(network) {
  return network.deactivate;
};

exports.resume = function resume(network) {
  return network.resume;
};

exports.suspend = function suspend(network) {
  return network.suspend;
};

exports.readLatch = function readLatch(latch) {
  return function readLatch_raff() {
    return function readLatch_eff() {
      return latch.read();
    };
  };
};

exports.networkTime = function networkTime(network) {
  return function networkTime_eff() {
    return network.timestamp;
  };
};

// Scheduler API

exports.timeoutScheduler = TimeoutScheduler();

exports.animationFrameScheduler = AnimationFrameScheduler();

// Raff API

exports.cached = function cached(raff) {
  const result = {};
  return function cached_raff(network) {
    return function cached_eff() {
      if (!result.hasOwnProperty("value")) {
        debug("Running cached raff", raff);
        result.value = raff(network)();
      }
      debug("Retrning cached raff result", result.value);
      return result.value;
    };
  };
};
