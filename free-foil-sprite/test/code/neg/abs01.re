/*@ val abs : x:int => int[v|0<=v] */
let abs = (x) => {
    let pos = x >= 0;
    if (pos) {
        x
    } else {
        0 - x
    }
};

/*@ val test : a:int => (b:int => int[v|0<=v && a + b <= v]) */
let test = (a) => { (b) => {
    let t1 = abs(a);
    let t2 = abs(b);
    t1 + t2
}};
let x = 0 - 5;
let y = 2;
let z = test(x);
z(2)
