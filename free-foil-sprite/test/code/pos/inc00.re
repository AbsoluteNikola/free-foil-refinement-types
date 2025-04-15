

/*@ val inc: x:int => int[v|v == x + 10] */
let inc = (x) => {
    let q = 10;
    q + x
};
inc(10)
