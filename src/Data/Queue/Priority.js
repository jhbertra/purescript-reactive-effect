function PriorityQueue({ just, nothing }) {
  const buckets = [];
  return {
    just,
    nothing,
    enqueue: function PriorityQueue_enqueue(priority, a) {
      buckets[priority] = buckets[priority] || [];
      buckets[priority].unshift(a);
    },
    dequeue: function PriorityQueue_dequeue() {
      for (let i = 0; i < buckets.length; ++i) {
        const bucket = buckets[i];
        if (bucket && bucket.length > 0) {
          return { priority: i, value: bucket.pop() };
        }
      }
    },
  };
}

const _new = (util) => () => PriorityQueue(util);

const enqueue = (priority) => (a) => (queue) => () =>
  queue.enqueue(priority, a);

const dequeue = (queue) => () => {
  const dequeued = queue.dequeue();
  return dequeued ? queue.just(dequeued) : queue.nothing;
};

const drain = (queue) => (f) => () => {
  let value = queue.dequeue();
  while (value) {
    f(value.priority)(value.value)();
    value = queue.dequeue();
  }
};

export { _new, enqueue, dequeue, drain };
