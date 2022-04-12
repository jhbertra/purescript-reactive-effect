"use strict";

const debugMode =
  localStorage.getItem("Effect.Reactive.Internal.debugMode") === "true";

const NODE_PULSE = debugMode ? "NODE_PULSE" : 0;

const NETWORK_OFFLINE = debugMode ? "NETWORK_OFFLINE" : 0;
const NETWORK_STANDBY = debugMode ? "NETWORK_STANDBY" : 1;
const NETWORK_PENDING_EVALUATION = debugMode ? "NETWORK_PENDING_EVALUATION" : 2;
const NETWORK_EVALUATING = debugMode ? "NETWORK_EVALUATING" : 3;

const debug = debugMode ? console.debug : () => {};
const log = debugMode ? console.log : () => {};
const warn = debugMode ? console.warn : () => {};

function SyncScheduler() {
  return {
    schedule: function syncSchedulerSchedule(task) {
      task();
    },
  };
}

function Pulse(id, connect) {
  debug("Pulse", id, connect);
  const pulse = { tag: NODE_PULSE, id };
  let connected = false;
  pulse.connect = function pulseConnect() {
    if (connected) return () => {};
    connected = true;
    debug("Pulse.connect", pulse);
    const disconnect = connect(pulse);
    return function pulseDisconnect() {
      if (connected) {
        debug("Pulse.disconnect", pulse);
        disconnect();
        connected = false;
      }
    };
  };
  pulse.dump = function () {
    return { id, connected };
  };
  return pulse;
}

function Graph() {
  const roots = new Set();
  const nodes = new Map();
  let nextNodeId = 0;
  return {
    newRootPulse: function graphNewPulse(connect) {
      debug("Graph.newRootPulse");
      return Pulse(nextNodeId++, function graphNewPulseConnect(pulse) {
        const ctx = "Graph.newRootPulse.connect";
        debug(ctx, pulse);
        roots.add(pulse.id);
        debug(ctx, "added pulse to roots", pulse, roots);
        nodes.set(pulse.id, pulse);
        debug(ctx, "added pulse to nodes", pulse, nodes);
        const disconnect = connect(pulse);
        return function graphNewPulseDisconnect() {
          const ctx = "Graph.newRootPulse.disconnect";
          debug(ctx, pulse);
          disconnect();
          nodes.delete(pulse.id);
          debug(ctx, "removed pulse from nodes", pulse, nodes);
          roots.delete(pulse.id);
          debug(ctx, "removed pulse from roots", pulse, roots);
        };
      });
    },
    dump: function graphDump() {
      return { roots, nodes, nextNodeId };
    },
  };
}

function NetworkInput(id, graph, connect, raise) {
  debug("NetworkInput", id, graph, connect, raise);
  const input = { id };
  let connected = false;
  const pulse = graph.newRootPulse(function networkInputConnect(pulse) {
    if (connected) return () => {};
    connected = true;
    debug("NetworkInput.connect", input, pulse);
    const disconnect = connect(input, pulse);
    return function networkInputDisconnect() {
      if (connected) {
        debug("NetworkInput.disconnect", input, pulse);
        disconnect();
        connected = false;
      }
    };
  });
  input.connect = pulse.connect;
  input.raise = function (a) {
    if (connected) {
      debug("NetworkInput.raise", input, a);
      raise(input, a);
    }
  };
  return input;
}

function Network(scheduler, graph) {
  let status = NETWORK_OFFLINE;
  let nextInputId = 0;
  let nextOutputId = 0;
  const inputs = new Map();
  const inputDisconnects = new Map();
  const pendingInputs = [];
  const inputsRaised = new Map();
  const outputs = new Map();
  const evaluators = new Map();

  function evaluate() {
    debug("Network.evaluate");
    status = NETWORK_EVALUATING;
    debug("Network.evaluate", "status set to", status);
    debug("Network.evaluate", "processing raised inputs", inputsRaised);
    // TODO
    inputsRaised.clear();
    debug("Network.evaluate", "inputsRaised cleared", inputsRaised);
    status = NETWORK_STANDBY;
    debug("Network.evaluate", "status set to", status);
  }

  return {
    getStatus: function () {
      return status;
    },

    newInput: function newInput(connect) {
      debug("Network.newInput", connect);
      const input = NetworkInput(
        nextInputId++,
        graph,
        function newInputConnect(input, pulse) {
          const ctx = "Network.newInput.connect";
          debug(ctx, input, pulse);
          inputs.set(input.id, pulse);
          debug(ctx, "added input to inputs", input, pulse, inputs);
          const disconnect = connect(input, pulse);
          return function () {
            const ctx = "Network.newInput.disconnect";
            debug(ctx, input, pulse);
            disconnect();
            inputs.delete(input.id);
            debug(ctx, "removed input from inputs", input, pulse, inputs);
          };
        },
        function newInputRaise(input, a) {
          const ctx = "Network.newInput.raise";
          debug(ctx, input, a);
          if (status === NETWORK_OFFLINE) return;
          if (status === NETWORK_EVALUATING) {
            debug(ctx, "scheduling raise");
            scheduler.schedule(function () {
              input.raise(a);
            });
          } else {
            inputsRaised.set(input.id, a);
          }
          if (status === NETWORK_STANDBY) {
            debug(ctx, "scheduling evaluation");
            status = NETWORK_PENDING_EVALUATION;
            scheduler.schedule(evaluate);
          }
        }
      );
      switch (status) {
        case NETWORK_OFFLINE:
          pendingInputs.unshift(input);
          break;
        default:
          inputDisconnects.unshift(input.connect());
          break;
      }
      return input.raise;
    },

    actuate: function () {
      debug("Network.actuate");
      switch (status) {
        case NETWORK_OFFLINE:
          debug("actuating network");
          status = NETWORK_STANDBY;
          debug("connecting pendingInputs", pendingInputs);
          while (pendingInputs.length > 0) {
            const input = pendingInputs.pop();
            inputDisconnects.set(input.id, input.connect());
          }
          break;
        default:
          warn(
            "***Effect.Reactive.Internal:",
            "actuate called on an already running Network"
          );
          break;
      }
    },

    deactivate: function () {
      debug("Network.deactivate");
      switch (status) {
        case NETWORK_OFFLINE:
          warn(
            "***Effect.Reactive.Internal:",
            "deactivate called on an already offline Network"
          );
          break;
        default:
          debug("deactivating network");
          debug("disconnecting inputs", inputs);
          for (const [key, input] of inputs.entries()) {
            const disconnect = inputDisconnects.get(key);
            if (disconnect) {
              disconnect();
            }
            pendingInputs.unshift(input);
          }
          inputs.clear();
          inputDisconnects.clear();
          status = NETWORK_OFFLINE;
          break;
      }
    },

    dump: function networkDump() {
      return {
        status,
        nextInputId,
        nextOutputId,
        inputs,
        inputDisconnects,
        inputsRaised,
        outputs,
        pendingInputs,
        evaluators,
      };
    },
  };
}

// function Raff(scheduler, network) {
//   return {};
// }
