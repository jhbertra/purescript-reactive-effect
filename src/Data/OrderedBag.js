"use strict";

exports.new = () => {
  return { nextPosition: 0, members: new Map() };
};

exports.insert = (member) => (bag) => () => {
  let position = bag.nextPosition++;
  bag.members.set(member, position);
  return position;
};

exports._get = (nothing) => (just) => (member) => (bag) => () => {
  if (bag.members.has(member)) {
    return just(bag.members.get(member));
  }
  return nothing;
};

exports.delete = (member) => (bag) => () => {
  bag.members.delete(member);
};
