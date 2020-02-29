const fs = require("fs");
const clc = require('cli-color');
const tw = process.stdout.columns; // total width
const deepEqual = require('deep-equal');

// EVM Instructions
const evm_i = require("./evm.json");


// HELPER
const hex = i => (s = i.toString(16)).length == 1 ? "0" + s : s;


const genSrcmapArr = srcmap => srcmap
  .split(";")
  .map(l => l.split(":"))
  .map(([s, l, f, j]) => ({ s: s === "" ? undefined : s, l, f, j }))
  .reduce(
     ([last, ...list], { s, l, f, j }) => [
      {
        s: parseInt(s || last.s, 10),
        l: parseInt(l || last.l, 10),
        f: parseInt(f || last.f, 10),
        j: j || last.j
      },
      last,
      ...list
    ],
    [{}]
  )
  .reverse()
  .slice(1)

// Instructions
const get_pc_to_inst_map = bin_string => {
  const code_arr = []
  for(let i=0; i<bin_string.length; i+=2) {
    code_arr.push(parseInt(bin_string.slice(i, i+2), 16));
  }
  const pc_to_inst_map = [];
  const instructions = [];
  let i = 0;
  while(i < code_arr.length) {
    let i_ = i;
    let index = hex(code_arr[i]);
    let instruction_string = evm_i[index] || `UNKNOWN (${index})`;

    if(code_arr[i] >= 96 && code_arr[i] <= 127) {
      let length = code_arr[i] - 95;
      instruction_string += " " + code_arr.slice(i + 1, i + 1 + length).map(s => hex(s)).join("");
      i += length;
    }
    i++;
    instructions.push(instruction_string);
    for(let j=i_; j < i; j++) {
      pc_to_inst_map.push(instructions.length - 1);
    }
  }
  return {
    pc_to_inst_map,
    instructions
  };
}

// EXPORT
// This mess deals with displaying the highlighted compact
// source code with lines.
// DONT MESS WITH THIS CODE IF YOU DON'T HAVE TO
const getCodeStringFromPc = (srcs, contract, pc, trim, isCreate = false) => {
  let hide_color = 244;
  let show_color = 253;
  let bin_runtime    =  isCreate ? contract.bin : contract.bin_runtime;
  let srcmap_runtime =  isCreate ? contract.srcmap : contract.srcmap_runtime;
  let cpos = isCreate ? contract.bin_srcmapArr[contract.bin_pc_to_inst_map[pc]] : contract.bin_runtime_srcmapArr[contract.bin_runtime_pc_to_inst_map[pc]];
  if (cpos == undefined) return ""

  let hide = str => clc.xterm(hide_color)(str.split("\n").join("\n"))
  let show = str => clc.xterm(show_color)(str.split("\n").join("\n"))

  let source = cpos.f == -1 ? "" : srcs[contract.srcs[cpos.f]];
  let pre  = source.slice(0, cpos.s);
  let root = source.slice(cpos.s, cpos.s + cpos.l);
  let post = source.slice(cpos.s + cpos.l)
  let lines = [];
  let MAX_S = 2;
  let pre_split      = pre.split("\n");
  let root_split     = root.split("\n")
  let pre_last_el    = pre_split[pre_split.length - 1];
  let pre_last_el_nl = (pre_last_el == "" ? 1 : 0);
  let post_split     = post.split("\n");
  let pre_rm         = MAX_S + 1;
  if(trim && pre.split("\n").length > pre_rm) {
    pre = pre_split
      .slice(-pre_rm)
      .join("\n")
    lines = lines
      .concat(["…"])
      .concat(pre.split("\n").map((e, i) => i + pre_split.length - pre_rm + 1))
    pre = "\n" + pre
  } else {
    lines = pre.split("\n").map((l, i) => i + 1)
  }
  if(trim && root.split("\n").length > MAX_S * 2 + 1) {
    let root_l = root_split
      .slice(0, MAX_S);
    let root_r = root_split
      .slice(-MAX_S)
    root = root_l
      .concat([root_split[MAX_S].match(/^\s*/)[0] + clc.xterm(hide_color)("[...]")])
      .concat(root_r)
      .join("\n")
    lines = lines
      .concat(root_l.slice(1).map((l, i) => pre_split.length + i + 1)) // l
      .concat(["…"])
      .concat(root_r.map((l, i) => pre_split.length + i + root_split.length - MAX_S))
  } else {
    lines = lines
      .concat(root_split.slice(1).map((l, i) => pre_split.length + i + 1))
  }
  if(trim && post_split.length > pre_rm) {
    let post_slice = post_split
      .slice(0, pre_rm)
    post = post_slice
      .concat(["\n"])
      .join("\n")
    lines = lines
      .concat(post_slice.slice(1).map((l, i) => pre_split.length + root_split.length + i))
      .concat(["…"])
  } else {
    lines = lines
      .concat(post_split.slice(1).map((l, i) => pre_split.length + root_split.length + i))
  }

  lines = lines.map(l => clc.bgXterm(236).xterm(show_color - 11)(" ".repeat(3 - l.toString().length) + l + " "))

  var pre_   = "pre: >" + pre_split.join(",") + "<\n"
  var root_  = "  rot: >" + root_split.join(",") + "<\n"
  var lines_ = "  lines: >" + lines.join(",") + "<\n"
  // var str = hide(pre) + show(root) + hide(post);
  var str = pre.split("\n").map(s => hide(s)).join("\n")
          + root.split("\n").map(s => show(s)).join("\n")
          + post.split("\n").map(s => hide(s)).join("\n")
  var split_str = str.split("\n")
  if(clc.getStrippedLength(split_str[split_str.length - 1]) == 0) split_str = split_str.slice(0, -1);
  str = split_str
    .map((l, i) => {
      let _l;
      return lines[i] + "   " + l;
    })
    .join("\n");
  // str = clc.bgXterm(236)(str);
  return str;
}

let getNodeSrcAst = (ast, name, args) => {
    return ast.map(node => {
      if (node.attributes && node.attributes.name && node.attributes.name == name) {
        let fArgTypes = node.children[0].children.map(s => s.attributes.type)
        if (deepEqual(fArgTypes,args))
        {
          return node.src;
        }
      }
      if (node.children) {
        return getNodeSrcAst(node.children, name, args).join('');
      }
    })
}

let getContractAst = (ast, contractName) => {
    return ast.map(node => {
        if (node.attributes && node.attributes.name) {
            if (node.attributes.name == contractName) {
                return node;
            }
        }
        if (node.children) {
            return getContractAst(node.children, contractName).join('');
        }
    })

}

let functionNameToPcRange = (sig, srcmapArr, ast, bin_runtime, inst_to_pc) => {
    let name = sig.split('(')[0]
    let args = sig.split('(')
      .slice(1).join('')
      .replace(')','')
        .split(',')
    let pcList = get_pc_to_inst_map(bin_runtime).pc_to_inst_map;
    let srcFromAst = getNodeSrcAst([ast], name, args)[0];
    let s_l_f = srcmapArr.map(e => [e.s, e.l, e.f].join(':'));
    let start_index = s_l_f.indexOf(srcFromAst)
    let pc_start = inst_to_pc[start_index]
    // explore all possible branches, and search for the first "out" jump
    // limited to internal functions of at most 100 bytes
    let max_search_depth = 100
    let branches = unrollBranches(bin_runtime, Number(pc_start), null, 0, max_search_depth)
    let j = srcmapArr.map(e => e.j)
    let pc_end = searchBreadthFirst(branches, (i => j[pcList[i]] == 'o'))
    return [pc_start, pc_end]
}

// virgin formal verification vs. chad static analysis
let unrollBranches = (bin, start_pc, pushed_pc, depth, max_depth) => {
    if (depth >= max_depth) {
        return [start_pc, []]
    }
    let start_off = 2 * start_pc
    let next_op = bin.slice(start_off, start_off + 2)
    let next_op_num = parseInt("0x" + next_op)
    if (next_op == "00" || // STOP
        next_op == "f3" || // RETURN
        next_op == "fd")   // REVERT
    {
        return [start_pc, []]
    }
    if (next_op == "56" || // JUMP
        next_op == "57")   // JUMPI
    {
        // here we branch
        return [start_pc, [unrollBranches(bin, pushed_pc, null, depth + 1, max_depth),
                           unrollBranches(bin, start_pc + 1, null, depth + 1, max_depth)]]
    }
    if (96 <= next_op_num && // PUSHn
        next_op_num <= 127) {
        let n = next_op_num - 95
        let pushing_pc = parseInt("0x" + bin.slice(start_off + 2, start_off + 2 * n + 2))
        return [start_pc, [unrollBranches(bin, start_pc + n + 1, pushing_pc, depth + 1, max_depth)]]
    }
    // all other opcodes
    return [start_pc, [unrollBranches(bin, start_pc + 1, null, depth + 1, max_depth)]]
}

let searchBreadthFirst = (tree, predicate) => {
    return toListBreadthFirst(tree).find(predicate)
}

let toListBreadthFirst = tree => {
    if (tree[1].length == 0) {
        if (tree[0] === null) {
            return []
        }
        else {
            return [tree[0]]
        }
    }
    // the multiplication of the list monad
    const flatten = ListOfList => ListOfList.reduce((a, list) => a.concat(list), [])
    let collapsedChildren = flatten(tree[1].map(x => {
        if (x.length == 0) { return [] }
        else { return x[1]}
    }))
    return ((x => {if (x === null) { return [] } else { return [x]}})(tree[0]))
        .concat(tree[1].map(x => x[0]))
        .concat(toListBreadthFirst([null, collapsedChildren]))
}

module.exports = {
  getCodeStringFromPc,
  get_pc_to_inst_map,
  genSrcmapArr,
  functionNameToPcRange,
  getContractAst
};
