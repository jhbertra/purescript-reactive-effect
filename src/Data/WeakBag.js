const _new = (cleanup) => () => {
  return {
    cleanup,
    nextKey: 0,
    members: new Map(),
    issuedTickets: new WeakMap(),
  };
};

const insert = (member) => (weakBag) => () => {
  if (weakBag.issuedTickets.has(member)) {
    return weakBag.issuedTickets.get(member);
  }
  let key = weakBag.nextKey++;
  weakBag.members.set(key, member);
  let ticket = {
    key,
    isDestroyed: false,
  };
  // avoid capturing member in a closure.
  ticket.destroy = destroyWeakBagTicket(weakBag, ticket);
  weakBag.issuedTickets.set(member, ticket);
  return ticket;
};

const get = (ticket) => (weakBag) => () => {
  if (ticket.isDestroyed) {
    throw new Error("WeakBagTicket is destroyed");
  }
  return weakBag.members.get(ticket.key);
};

const destroyTicket = ({ destroy }) => destroy;

const members =
  ({ members }) =>
  () =>
    Array.from(members.values());

function destroyWeakBagTicket(weakBag, ticket) {
  return function () {
    weakBag.members.delete(ticket.key);
    ticket.isDestroyed = true;
    ticket.destroy = function () {
      throw new Error("WeakBagTicket is destroyed");
    };
    if (weakBag.members.size === 0) {
      weakBag.cleanup();
    }
  };
}

export { _new, insert, get, destroyTicket, members };
