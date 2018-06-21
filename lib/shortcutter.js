const fs = require("fs");

//TEST

let pre = "/private/var/folders/fb/0m3l_cnn4w57fhsv7q_lx5780000gn/T/klab/cfb53897/nodes/1103642064.json"
let post = "/private/var/folders/fb/0m3l_cnn4w57fhsv7q_lx5780000gn/T/klab/cfb53897/nodes/563029515.json"


const getTerm = nodePath => {
    let read = fs.readFileSync(nodePath).toString()
    return JSON.parse(read).term
}

const cleanToken = token => {
    //make wildcards into wildcards
    if (token[0] == '_') {
        return '_';
    }
    return token.replace(/\.\_\d\d+:\w+/g, " ")
}
    

const writeCell = (preCell, postCell) => {
    //Leafs have cell.node: "KToken"
    if (preCell.node == "KToken") {
        return cleanToken(preCell.token) + " => " + cleanToken(postCell.token);
    }
    //anything else should be a KApply
    //sanity check: the precell has the same format as the postcell
    if (preCell.label != postCell.label) {
        throw "prestate and poststate have different term structure";
    }
    let result = preCell.label + " ";
    for (let i = 0; i < preCell.arity; i++) {
        result = result + writeCell(preCell.args[i], postCell.args[i]);
    }
    return '\n' + result + "\n" + preCell.label.replace('<','</');
}

const generateRewrite = (prePath, postPath) => {
    return "rule" + '\n' + writeCell(
        getTerm(prePath), getTerm(postPath))
    //todo attribute: trusted
}

module.exports = generateRewrite
