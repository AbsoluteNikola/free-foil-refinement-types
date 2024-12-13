

/*@ val inc: x:int[x|true] => int[v|v == x ] */
let inc = (x) => {
    let q = 10;
    q + x
};
inc(10)
