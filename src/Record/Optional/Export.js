export const jsNull = null;

export const isNull = function (a) {
  return a === null;
};

export const unsafeLog = (label) => (x) => {
  console.log(label, x);
  return x;
};
