const fs = require("fs");

let pre = "/private/var/folders/fb/0m3l_cnn4w57fhsv7q_lx5780000gn/T/klab/cfb53897/nodes/1103642064.json"
let post = "/private/var/folders/fb/0m3l_cnn4w57fhsv7q_lx5780000gn/T/klab/cfb53897/nodes/563029515.json"


const getTerm = nodePath => {
    let read = fs.readFileSync(nodePath).toString()
    return JSON.parse(read).term
}

const writeCell = (preCell, postCell) => {
    //Leafs have cell.node: "KToken"
    if (preCell.node == "KToken") {
        return preCell.token + " => " + postCell.token
    }
    let result = preCell.label + " ";
    for (let i = 0; i < preCell.arity; i++) {
        result = result + writeCell(preCell.args[i], postCell.args[i]);
        if (i > 0) {
            result = result + '\n';
        }
    }
    return result + " " + preCell.label.replace('<','</');
}

const rewrite = (prePath, postPath) => {
    return "rule:" + '\n' + writeCell(
        getTerm(prePath), getTerm(postPath))
}

test();
