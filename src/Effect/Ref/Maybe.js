"use strict";

exports.new = function (val) {
  return function () {
    return val.hasOwnProperty("value0") ? { value: val.value0 } : {};
  };
};

exports.newWithSelf = function (f) {
  return function () {
    var ref = {};
    var val = f(ref);
    if (val.hasOwnProperty("value0")) {
      ref.value = val.value0;
    }
    return ref;
  };
};

exports._read = function ({ nothing, just }) {
  return function (ref) {
    return function () {
      return ref.hasOwnProperty("value") ? just(ref.value) : nothing;
    };
  };
};

exports.modifyImpl = function (f) {
  return function (ref) {
    return function () {
      var t = f(ref.value);
      ref.value = t.state;
      return t.value;
    };
  };
};

exports.write = function (val) {
  return function (ref) {
    return function () {
      ref.value = val;
    };
  };
};

exports.clear = function (ref) {
  return function () {
    delete ref.value;
  };
};

exports.isFilled = function (ref) {
  return function () {
    return ref.hasOwnProperty("value");
  };
};
