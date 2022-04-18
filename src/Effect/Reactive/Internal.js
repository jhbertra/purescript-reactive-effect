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
    getScheduleTime: function AnimationFrameScheduler_getScheduleTime() {
      return performance.now() - offset;
    },
    schedule: function AnimationFrameScheduler_schedule(task) {
      requestAnimationFrame(function AnimationFrameScheduler_run(timestamp) {
        task(timestamp - offset);
      });
    },
  };
}

function TestScheduler() {
  let time = 0;
  const queue = [];
  return {
    reset: function TestScheduler_reset() {
      time = 0;
      queue.splice(0);
    },
    getScheduleTime: function TestScheduler_getScheduleTime() {
      return time;
    },
    schedule: function TestScheduler_schedule(task) {
      queue.unshift(task);
    },
    flush: function TestScheduler_flush() {
      while (queue.length > 0) {
        queue.pop()(time);
      }
      time++;
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
    getScheduleTime: function TimeoutScheduler_getScheduleTime() {
      performance.measure(measurement, marker);
      const [entry] = performance.getEntriesByName(measurement);
      performance.clearMeasures(measurement);
      return entry.duration;
    },
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
  const inputs = new Map();
  const outputs = new Map();
  const parents = new Map();
  const children = new Map();

  self.id = id;

  self.traverseChildren = function Node_traverseChildren(f) {
    traverse(children.values(), f);
  };

  self.traverseParents = function Node_traverseParents(f) {
    traverse(parents.values(), f);
  };

  function isConnected() {
    return inputs.size > 0 && outputs.size > 0;
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
      const count = inputs.get(input) || 0;
      inputs.set(input, count + 1);
      self.traverseChildren((c) => c.addInput(input));
    });
  };

  self.removeInput = function Node_removeInput(id) {
    updateIO(function () {
      const count = inputs.get(id);
      if (count) {
        const newCount = count - 1;
        if (newCount) {
          inputs.set(id, count + 1);
        } else {
          inputs.delete(id);
        }
        self.traverseChildren((c) => c.removeInput(id));
      }
    });
  };

  self.addOutput = function Node_addOutput(output) {
    debug("Node.addOutput", self.id, output);
    updateIO(function () {
      const count = outputs.get(output) || 0;
      outputs.set(output, count + 1);
      self.traverseParents((p) => p.addOutput(output));
    });
  };

  self.removeOutput = function Node_removeOutput(id) {
    updateIO(function () {
      const count = outputs.get(id);
      if (count) {
        const newCount = count - 1;
        if (newCount) {
          outputs.set(id, count + 1);
        } else {
          outputs.delete(id);
        }
        self.traverseParents((p) => p.removeOutput(id));
      }
    });
  };

  self.addParent = function Node_addParent(parent) {
    if (parents.has(parent.id)) return;
    debug("Node.addParent", self.id, parent.id);
    parents.set(parent.id, parent);
    parent.addChild(self);
    traverse(outputs.keys(), parent.addOutput);
  };

  self.removeParent = function Node_removeParent(parent) {
    parents.delete(parent.id);
    parent.removeChild(self);
  };

  self.addChild = function Node_addChild(child) {
    if (children.has(child.id)) return;
    debug("Node.addChild", self.id, child.id);
    children.set(child.id, child);
    child.addParent(self);
    traverse(inputs.keys(), child.addInput);
  };

  self.removeChild = function Node_removeChild(child) {
    children.delete(child.id);
    traverse(inputs.keys(), child.removeInput);
  };

  self.fire = function Node_fire(value) {
    debug("Node.fire", id, value, inputs);
    fire(value, self);
  };

  return self;
}

function InputNode(id, fire) {
  const self = Node(id, fire);
  self.addInput(self.id);
  delete self.addParent;
  delete self.addInput;
  delete self.removeParent;
  return self;
}

function OutputNode(id, fire) {
  const self = Node(id, fire);
  self.addOutput(self.id);
  delete self.addChild;
  delete self.removeChild;
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

function Circuit(id, inputs, outputs, fire) {
  const self = Node(id, fire);
  Object.values(inputs).forEach(self.addParent);
  Object.values(outputs).forEach(self.addChild);
  return self;
}

function Animation(id, scheduler) {
  const self = new EventTarget();
  let requestId;
  self.setAnimation = function Animation_setAnimation(animate) {
    function Animation_setAnimate_animateFrame() {
      animate(scheduler.getScheduleTime())();
      requestId = requestAnimationFrame(Animation_setAnimate_animateFrame);
    }
    self.clearAnimation();
    requestId = requestAnimationFrame(Animation_setAnimate_animateFrame);
  };
  self.clearAnimation = function Animation_clearAnimation() {
    if (requestId) {
      cancelAnimationFrame(requestId);
    }
  };
  return self;
}

function Network(Just, Nothing, scheduler) {
  const self = new EventTarget();
  let nextNodeId = 0;
  let nextAnimationId = 0;
  const latchWrites = new Map();
  const nodeValues = new Map();
  const raisedInputs = new Map();
  const raisedCircuits = new Map();
  const raisedOutputs = new Map();
  const animations = new Map();

  self.status = NETWORK_OFFLINE;
  self.timestamp = Number.NEGATIVE_INFINITY;

  function propagateToChildren(node, value) {
    nodeValues.set(node.id, value);
    node.traverseChildren((c) => c.fire(value));
  }

  function evaluate(timestamp) {
    self.timestamp = timestamp;
    debug("Network.evaluate");
    self.status = NETWORK_EVALUATING;
    flushInputs();
    evaluateCircuits();
    flushLatchWrites();
    self.status = NETWORK_EMITTING;
    flushOutputs();
    debug("Network.evaluate - done");
    self.status = NETWORK_STANDBY;
  }

  function flushInputs() {
    if (self.status === NETWORK_OFFLINE || raisedInputs.size == 0) return;
    traverse(
      raisedInputs.values(),
      function Network_flushInputs_flushInput({ value, input }) {
        if (self.status === NETWORK_OFFLINE) return;
        debug("Network.flushInputs", "flushing input", input.id, value);
        propagateToChildren(input, value);
      }
    );
    raisedInputs.clear();
  }

  function evaluateCircuits() {
    while (raisedCircuits.size > 0) {
      const nodes = Array.from(raisedCircuits, function ([id, evalCircuit]) {
        debug("Network.evaluateCircuits.process", id);
        return evalCircuit;
      });
      raisedCircuits.clear();
      nodes.forEach(function Network_evaluateCircuits_process(evalCircuit) {
        if (self.status === NETWORK_OFFLINE) return;
        evalCircuit();
      });
    }
    nodeValues.clear();
  }

  function flushLatchWrites() {
    traverse(
      latchWrites.values(),
      function Network_evaluate_writeLatch({ value, latch }) {
        if (self.status === NETWORK_OFFLINE) return;
        debug("Network.writeLatch", latch.id, value);
        latch.write(value);
      }
    );
    latchWrites.clear();
  }

  function flushOutputs() {
    if (self.status === NETWORK_OFFLINE || raisedOutputs.size == 0) return;
    traverse(raisedOutputs.entries(), function ([id, { value, sink }]) {
      debug("Network.emit", id, value);
      sink(value);
    });
    raisedOutputs.clear();
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
  self.empty.traverseChildren = function Network_empty_traverseChildren() {};
  self.empty.traverseParents = function Network_empty_traverseParents() {};
  self.empty.addInput = function Network_empty_addInput() {};
  self.empty.removeInput = function Network_empty_removeInput() {};
  self.empty.addParent = function Network_empty_addParent() {};
  self.empty.removeParent = function Network_empty_removeParent() {};
  self.empty.addChild = function Network_empty_addChild() {};
  self.empty.removeChild = function Network_empty_removeChild() {};
  self.empty.fire = function Network_empty_fire() {};

  self.ground = newNode(function Network_ground_makeNode(id) {
    return InputNode(id, function Network_ground_fire() {});
  });

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
      return InputNode(id, function Network_newInput_fire(value, input) {
        switch (self.status) {
          case NETWORK_STANDBY:
            self.status = NETWORK_SCHEDULED;
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
      return OutputNode(id, function Network_newOutput_fire(value) {
        switch (self.status) {
          case NETWORK_EVALUATING:
            raisedOutputs.set(id, { value, sink });
            break;
        }
      });
    });
  };

  self.newAnimation = function Network_newAnimation() {
    debug("Network.newAnimation");
    const animation = Animation(nextAnimationId++, scheduler);
    return {
      setAnimation: function Network_newAnimation_setAnimation(animate) {
        if (
          self.status !== NETWORK_OFFLINE &&
          self.status !== NETWORK_SUSPENDED
        ) {
          animation.setAnimation(animate);
        }
        animations.set(animation.id, { animation, animate });
      },
      clearAnimation: function Network_newAnimation_clearAnimation() {
        animation.clearAnimation();
        animations.delete(animation.id);
      },
    };
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
              propagateToChildren(node, value);
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
            propagateToChildren(node, outValue);
            break;
        }
      });
    });
  };

  self.newProcess = function Network_newProcess(evalProcess) {
    debug("Network.newProcess");
    return newNode(function Network_newProcess_makeNode(id) {
      return Node(id, function Network_newProcess_fire(inValue, node) {
        switch (self.status) {
          case NETWORK_EVALUATING:
            function Network_newProcess_write(outValue) {
              return function Network_newProcess_write_raff(network) {
                return function Network_newProcess_write_eff() {
                  propagateToChildren(node, outValue);
                };
              };
            }
            evalProcess(Network_newProcess_write)(inValue)(self)();
            break;
        }
      });
    });
  };

  self.newCircuit = function Network_newCircuit(inputs, outputs, evalCircuit) {
    debug("Network.newCircuit");
    const inputReads = {};
    const outputWrites = {};
    Object.entries(inputs).forEach(function ([k, inNode]) {
      function Network_newCircuit_readInput_raff(network) {
        return function Network_newCircuit_readInput_eff() {
          return network.readNode(inNode.id);
        };
      }
      inputReads[k] = Network_newCircuit_readInput_raff;
    });
    Object.entries(outputs).forEach(function ([k, outNode]) {
      function Network_newCircuit_writeOutput(value) {
        return function Network_newCircuit_writeOutput_raff(network) {
          return function Network_newCircuit_writeOutput_eff() {
            outNode.fire(value);
          };
        };
      }
      outputWrites[k] = Network_newCircuit_writeOutput;
    });
    const evalCircuitEff = evalCircuit(inputReads)(outputWrites)(self);
    return newNode(function Network_newCircuit_makeNode(id) {
      return Circuit(
        id,
        inputs,
        outputs,
        function Network_newCircuit_fire(_, node) {
          switch (self.status) {
            case NETWORK_EVALUATING:
              raisedCircuits.set(id, evalCircuitEff);
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
      traverse(
        animations.values(),
        function stopAnimation({ animation, animate }) {
          animation.setAnimation(animate);
        }
      );
    }
  };

  self.deactivate = function Network_deactivate() {
    if (self.status !== NETWORK_OFFLINE) {
      self.status = NETWORK_OFFLINE;
      self.timestamp = Number.NEGATIVE_INFINITY;
      self.dispatchEvent(new Event("deactivated"));
      raisedInputs.clear();
      raisedCircuits.clear();
      latchWrites.clear();
      nodeValues.clear();
      raisedOutputs.clear();
      traverse(animations.values(), function stopAnimation({ animation }) {
        animation.clearAnimation();
      });
      animations.clear();
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
        traverse(animations.values(), function stopAnimation({ animation }) {
          animation.clearAnimation();
        });
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
      traverse(
        animations.values(),
        function stopAnimation({ animation, animate }) {
          animation.setAnimation(animate);
        }
      );
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

exports.ground = function ground(network) {
  return function () {
    return network.ground;
  };
};

exports.newInput = function newInput(network) {
  return network.newInput;
};

exports.newBuffer = function newBuffer(network) {
  return network.newBuffer;
};

exports.newAnimation = function newAnimation(network) {
  return network.newAnimation;
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

exports.newProcess = function newProcess(evalProcess) {
  return function newProcess_raff(network) {
    return function newProcess_eff() {
      return network.newProcess(evalProcess);
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

exports._newCircuit = function newCircuit(inputs, outputs, evalCircuit) {
  return function newCircuit_raff(network) {
    return function newCircuit_eff() {
      return network.newCircuit(inputs, outputs, evalCircuit);
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

exports._readNode = function readNode(node) {
  return function readNode_raff(network) {
    return function readNode_eff() {
      return network.readNode(node.id);
    };
  };
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

// Animation API

exports._setAnimation = function setAnimation(animation, animate) {
  animation.setAnimation(animate);
  return function clearAnimation() {
    animation.clearAnimation();
  };
};

// Scheduler API

exports.newTimeoutScheduler = TimeoutScheduler;

exports.newAnimationFrameScheduler = AnimationFrameScheduler;

exports.newTestScheduler = TestScheduler;

exports.flushTestScheduler = function flushTestScheduler(testScheduler) {
  return function flushTestScheduler_eff() {
    testScheduler.flush();
  };
};

// Raff API

exports.cached = function cached(raff) {
  const result = {};
  return function cached_raff(network) {
    return function cached_eff() {
      if (!result.hasOwnProperty("value")) {
        result.value = raff(network)();
      }
      return result.value;
    };
  };
};
