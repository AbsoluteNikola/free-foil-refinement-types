
/*@ val inc: x:int => int[v|v == x + 1] */
let inc = (x) => {
    x + 1
};

/*@ val inc2: x:int => int[v|v == x + 2] */
let inc2 = (x) => {
    let tmp = inc(x);
    inc(tmp)
};
inc2(10)
