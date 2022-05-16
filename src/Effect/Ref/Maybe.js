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

const modifyImpl = function (def) {
  return function (f) {
    return function (ref) {
      return function () {
        var t;
        if (ref.hasOwnProperty("value")) {
          t = f(ref.value);
        } else {
          t = def();
        }
        if (t.hasOwnProperty("value0")) {
          ref.value = t.state.value0;
        } else {
          delete ref.value;
        }
        return t.value;
      };
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
