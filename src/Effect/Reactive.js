export function getHighResTimestamp() {
  return performance.now();
}

export function requestAnimationFrame(f) {
  return function (window) {
    return function () {
      return window.requestAnimationFrame((time) => f(time)());
    };
  };
}
