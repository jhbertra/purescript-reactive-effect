const _new = function (val) {
  return function () {
    return val.hasOwnProperty("value0") ? { value: val.value0 } : {};
  };
};

const newWithSelf = function (f) {
  return function () {
    var ref = {};
    var val = f(ref);
    if (val.hasOwnProperty("value0")) {
      ref.value = val.value0;
    }
    return ref;
  };
};

const _read = function ({ nothing, just }) {
  return function (ref) {
    return function () {
      return ref.hasOwnProperty("value") ? just(ref.value) : nothing;
    };
  };
};

const modifyImpl = function (f) {
  return function (ref) {
    return function () {
      var t = f(ref.value);
      ref.value = t.state;
      return t.value;
    };
  };
};

const write = function (val) {
  return function (ref) {
    return function () {
      ref.value = val;
    };
  };
};

const clear = function (ref) {
  return function () {
    delete ref.value;
  };
};

const isFilled = function (ref) {
  return function () {
    return ref.hasOwnProperty("value");
  };
};

export { _new, newWithSelf, _read, modifyImpl, write, clear, isFilled };
