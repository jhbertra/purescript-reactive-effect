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

exports._new = (util) => () => PriorityQueue(util);

exports.enqueue = (priority) => (a) => (queue) => () =>
  queue.enqueue(priority, a);

exports.dequeue = (queue) => () => {
  const dequeued = queue.dequeue();
  return dequeued ? queue.just(dequeued) : queue.nothing;
};

exports.drain = (queue) => (f) => () => {
  let value = queue.dequeue();
  while (value) {
    f(value.priority)(value.value)();
    value = queue.dequeue();
  }
};
