"use strict";

// module Control.Monad.Fix

var message =
  "Control.Monad.Fix: Premature access to result of fixpoint computation.";

// fixEffect :: forall a. ((Unit -> a) -> Effect a) -> Effect a
exports.fixEffect = function (f) {
  return function () {
    var result = null;
    var ready = false;

    result = f(function (u) {
      if (!ready) throw new Error(message);
      return result;
    })();

    ready = true;
    return result;
  };
};

// fixPure :: forall a. ((Unit -> a) -> a) -> a
exports.fixPure_ = function (f) {
  return exports.fixEffect(function (a) {
    return function () {
      return f(a);
    };
  })();
};
