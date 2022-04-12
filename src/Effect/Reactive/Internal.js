"use strict";

const debugMode =
  localStorage.getItem("Effect.Reactive.Internal.debugMode") === "true";

const NODE_PULSE = debugMode ? "NODE_PULSE" : 0;

const NETWORK_UNMOUNTED = debugMode ? "NETWORK_UNMOUNTED" : 0;
const NETWORK_OFFLINE = debugMode ? "NETWORK_OFFLINE" : 1;
const NETWORK_STANDBY = debugMode ? "NETWORK_STANDBY" : 2;
const NETWORK_PENDING_EVALUATION = debugMode ? "NETWORK_PENDING_EVALUATION" : 3;
const NETWORK_EVALUATING = debugMode ? "NETWORK_EVALUATING" : 4;

const GRAPH_UNMOUNTED = debugMode ? "GRAPH_UNMOUNTED" : 0;
const GRAPH_DISCONNECTED = debugMode ? "GRAPH_DISCONNECTED" : 1;
const GRAPH_CONNECTED = debugMode ? "GRAPH_CONNECTED" : 2;

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

function Pulse(id, onMount) {
  debug("Pulse", id, onMount);
  const pulse = { tag: NODE_PULSE, id };
  let handlers = null;
  let onDisconnect = null;
  pulse.mount = function pulseMount() {
    if (handlers) return;
    debug("Pulse.mount", pulse);
    handlers = onMount(pulse);
  };
  pulse.connect = function pulseConnect() {
    if (!handlers || !handlers.onConnect || onDisconnect) return;
    debug("Pulse.connect", pulse);
    onDisconnect = handlers.onConnect(pulse);
  };
  pulse.disconnect = function () {
    if (!onDisconnect) return;
    debug("Pulse.disconnect", pulse);
    onDisconnect(pulse);
    onDisconnect = null;
  };
  pulse.unmount = function () {
    if (!handlers || !handlers.onUnmount || onDisconnect) return;
    debug("Pulse.unmount", pulse);
    handlers.onUnmount(pulse);
  };
  pulse.dump = function () {
    return { handlers, onDisconnect };
  };
  return pulse;
}

function Graph() {
  const roots = new Set();
  const nodes = new Map();
  let nextNodeId = 0;
  let status = GRAPH_UNMOUNTED;
  return {
    newRootPulse: function graphNewPulse(onMount) {
      const ctx = "Graph.newRootPulse";
      debug(ctx, onMount);
      const pulse = Pulse(nextNodeId++, function graphNewPulseOnMount(pulse) {
        const ctx = "Graph.newRootPulse.onMount";
        debug(ctx, pulse);
        const handlers = onMount(pulse);
        return {
          onConnect: handlers.onConnect,
          onUnmount: function graphNewPulseOnUnmount(pulse) {
            const ctx = "Graph.newRootPulse.onUnmount";
            debug(ctx, pulse);
            handlers.onUnmount();
            nodes.delete(pulse.id);
            debug(ctx, "removed pulse from nodes", pulse, nodes);
            roots.delete(pulse.id);
            debug(ctx, "removed pulse from roots", pulse, roots);
          },
        };
      });
      nodes.set(pulse.id, pulse);
      debug(ctx, "added pulse to nodes", pulse, nodes);
      roots.add(pulse.id);
      debug(ctx, "added pulse to roots", pulse, roots);
      switch (status) {
        case GRAPH_UNMOUNTED:
          break;
        default:
          pulse.mount();
      }
    },
    mount: function graphMount() {
      if (status !== GRAPH_UNMOUNTED) return;
      debug("Graph.mount");
      for (const entry of nodes) {
        const node = entry[1];
        node.mount();
      }
      status = GRAPH_DISCONNECTED;
    },
    connect: function graphConnect() {
      if (status !== GRAPH_DISCONNECTED) return;
      debug("Graph.connect");
      for (const entry of nodes) {
        const node = entry[1];
        node.connect();
      }
      status = GRAPH_CONNECTED;
    },
    disconnect: function graphDisconnect() {
      if (status !== GRAPH_CONNECTED) return;
      debug("Graph.disconnect");
      for (const entry of nodes) {
        const node = entry[1];
        node.disconnect();
      }
      status = GRAPH_DISCONNECTED;
    },
    unmount: function graphUnmount() {
      if (status !== GRAPH_DISCONNECTED) return;
      debug("Graph.unmount");
      for (const entry of nodes) {
        const node = entry[1];
        node.unmount();
      }
      status = GRAPH_UNMOUNTED;
    },
    dump: function graphDump() {
      return { roots, nodes, nextNodeId, status };
    },
  };
}

function NetworkOutput(id, graph, connect, sink) {
  debug("NetworkOutput", id, graph, connect, raise);
  const output = { id };
  let connected = false;
  const pulse = graph.newLeafPulse(function networkOutputConnect(pulse) {
    if (connected) return () => {};
    connected = true;
    debug("NetworkOutput.connect", output, pulse);
    const disconnect = connect(output, pulse);
    return function networkOutputDisconnect() {
      if (connected) {
        debug("NetworkOutput.disconnect", output, pulse);
        disconnect();
        connected = false;
      }
    };
  });
  output.sink = function (a) {
    if (connected) {
      debug("NetworkOutput.sink", output, a);
      sink(output, a);
    }
  };
  return output;
}

function Network(scheduler, graph) {
  let status = NETWORK_UNMOUNTED;
  let nextInputId = 0;
  let nextOutputId = 0;
  const inputs = new Map();
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

    newInput: function newInput(onMount) {
      debug("Network.newInput", onMount);
      const id = nextInputId++;
      let connected = false;
      graph.newRootPulse(function networkInputOnMount(pulse) {
        const ctx = "Network.newInput.onMount";
        debug(ctx, id, pulse);
        inputs.set(id, pulse);
        debug(ctx, "added input to inputs", id, pulse, inputs);
        const handlers = onMount(pulse);
        return {
          onConnect: function networkNewInputOnConnect(pulse) {
            const ctx = "Network.newInput.onConnect";
            debug(ctx, id, pulse);
            connected = true;
            const onDisconnect = handlers.onConnect(pulse);
            return function networkNewInputOnDisconnect(pulse) {
              const ctx = "Network.newInput.onDisconnect";
              debug(ctx, id, pulse);
              connected = false;
              onDisconnect(pulse);
            };
          },
          onUnmount: function networkNewInputOnUnmount(pulse) {
            const ctx = "Network.newInput.onUnmount";
            debug(ctx, id, pulse);
            handlers.onUnmount(pulse);
            inputs.delete(id);
            debug(ctx, "removed input from inputs", id, pulse, inputs);
          },
        };
      });

      function raise(a) {
        if (!connected) return;
        const ctx = "Network.newInput.raise";
        debug(ctx, id, a);
        if (status === NETWORK_OFFLINE) return;
        if (status === NETWORK_EVALUATING) {
          debug(ctx, "scheduling raise");
          scheduler.schedule(function () {
            raise(a);
          });
        } else {
          inputsRaised.set(id, a);
        }
        if (status === NETWORK_STANDBY) {
          debug(ctx, "scheduling evaluation");
          status = NETWORK_PENDING_EVALUATION;
          scheduler.schedule(evaluate);
        }
      }

      return raise;
    },

    mount: function networkMount() {
      debug("Network.mount");
      switch (status) {
        case NETWORK_UNMOUNTED:
          debug("mounting network");
          status = NETWORK_OFFLINE;
          graph.mount();
          break;
        default:
          warn(
            "***Effect.Reactive.Internal:",
            "mount called on an already mounted Network"
          );
          break;
      }
    },

    actuate: function () {
      debug("Network.actuate");
      switch (status) {
        case NETWORK_OFFLINE:
          debug("actuating network");
          status = NETWORK_STANDBY;
          graph.connect();
          break;
        case NETWORK_UNMOUNTED:
          warn(
            "***Effect.Reactive.Internal:",
            "actuate called without first calling mount"
          );
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
        case NETWORK_UNMOUNTED:
        case NETWORK_OFFLINE:
          warn(
            "***Effect.Reactive.Internal:",
            "deactivate called on an already offline Network"
          );
          break;
        default:
          debug("deactivating network");
          graph.disconnect();
          status = NETWORK_OFFLINE;
          break;
      }
    },

    unmount: function networkUnmount() {
      debug("Network.unmount");
      switch (status) {
        case NETWORK_OFFLINE:
          debug("unmounting network");
          status = NETWORK_UNMOUNTED;
          graph.unmount();
          break;
        default:
          warn(
            "***Effect.Reactive.Internal:",
            "mount called from inappropriate status",
            status
          );
          break;
      }
    },

    dump: function networkDump() {
      return {
        status,
        nextInputId,
        nextOutputId,
        inputs,
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
