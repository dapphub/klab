const fs = require("fs");

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

const writeConstraints = term => {
    let reqs = term.args.map(s => cleanToken(s.token));
    reqs = reqs.toString().replace(/,/g,' andBool\n');
    return reqs.slice(0,reqs.length-9);
}

const generateRewrite = (prePath, postPath, constraints) => {
    return "rule" + '\n' + writeCell(
        getTerm(prePath), getTerm(postPath)) + '\n' +
        "requires" + '\n' + writeConstraints(getTerm(constraints))
    //todo attribute: trusted
}

module.exports = generateRewrite
