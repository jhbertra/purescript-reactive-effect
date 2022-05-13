const _new = () => [];

const enqueue = (a) => (queue) => () => queue.unshift(a);

const _dequeue =
  ({ just, nothing }) =>
  (queue) =>
  (f) =>
  () => {
    if (queue.length) {
      return f(just(queue.pop()));
    }
    return f(nothing);
  };

const drain = (queue) => (f) => () => {
  while (queue.length) {
    f(queue.pop())();
  }
};

const toArray = (queue) => () => Array.from(queue);

export { _new, enqueue, _dequeue, drain, toArray };
