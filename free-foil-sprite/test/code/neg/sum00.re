/*@ val sum : n:int => int[v|0 <= v && n <= v] */
let rec sum = (n) => {
    let cond = n <= 0;
    if (cond) {
        0
    } else {
        let n1 = n-1;
        let t1 = sum(n1);
        t1
    }
};
0
