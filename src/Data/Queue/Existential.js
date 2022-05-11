exports.new = () => [];

exports.enqueue = (a) => (queue) => () => queue.unshift(a);

exports._dequeue =
  ({ just, nothing }) =>
  (queue) =>
  (f) =>
  () => {
    if (queue.length) {
      return f(just(queue.pop()));
    }
    return f(nothing);
  };

exports.drain = (queue) => (f) => () => {
  while (queue.length) {
    f(queue.pop())();
  }
};

exports.toArray = (queue) => () => Array.from(queue);
