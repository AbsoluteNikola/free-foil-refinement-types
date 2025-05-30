/*@ val cassert : arg0:bool[b|b] => int */
let cassert = (b) => { 0 };

/*@ val abs : x:int => int[?] */
let abs = (x) => {
  let pos = x >= 0;
  if (pos) {
    x
  } else {
    0 - x
  }
};

/*@ val incf: arg0:int => int */
let incf = (z) => {
  /*@ val wrap : arg0:(arg0:int => int[?]) => int[?]  */
  let wrap = (f) => {
    let r = f(z);
    r + 1
  };
  let res = wrap(abs);
  let ok  = 0 <= res;
  cassert (ok)
};
incf(6)
