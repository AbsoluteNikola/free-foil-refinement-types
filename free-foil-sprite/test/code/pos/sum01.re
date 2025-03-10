/**@ qualif pos(v:int):        (0 <= v) */
/**@ qualif geq(v:int, n:int): (n <= v) */

/*@ val cassert : b:bool[b|b] => int[v|true] */
let cassert = (b) => { 0 };

/*@ val sum : n:int[v|true] => int[?] */
let rec sum = (n) => {
    let cond = n <= 0;
    if (cond) {
        0
    } else {
        let n1 = n-1;
        let t1 = sum(n1);
        n + t1
    }
};

/*@ val check1 : arg0:int[v|true] => int[v|true] */
let check1 = (y) => {
  let res  = sum(y);
  let ok   = 0 <= res;
  cassert(ok)
};

/*@ val check2 : arg0:int[v|true] => int[v|true] */
let check2 = (y) => {
  let res = sum(y);
  let ok  = y <= res;
  cassert(ok)
};
check2(1)
