const _new = () => {
  return { nextPosition: 0, members: new Map() };
};

const insert = (member) => (bag) => () => {
  let position = bag.nextPosition++;
  bag.members.set(member, position);
  return position;
};

const _get = (nothing) => (just) => (member) => (bag) => () => {
  if (bag.members.has(member)) {
    return just(bag.members.get(member));
  }
  return nothing;
};

const _delete = (member) => (bag) => () => {
  bag.members.delete(member);
};

const toArray = (bag) => () => {
  return Array.from(bag.keys());
};

export { _new, insert, _get, _delete, toArray };
