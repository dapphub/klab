const {br, span} = require('@cycle/dom')
// const fs = require("fs");
// TODO - remove this - super hakky currently
const tw = process.stdout && process.stdout.columns || 300; // total width

// EVM Instructions
const evm_i = require("./evm.json");


// HELPER
const hex = i => (s = i.toString(16)).length == 1 ? "0" + s : s;
const flatten = (a, e) => a.concat(e)


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
const getCodeStringFromPc = (srcs, contract, pc, trim) => {
  let hide_color = 244;
  let show_color = 253;

  let bin_runtime    =  contract.bin_runtime;
  let srcmap_runtime =  contract.srcmap_runtime;
  var cpos = contract.srcmapArr[contract.pc_to_inst_map[pc]];
  let hide = str => span(".hide", [str])
  let show = str => span(".show", [str])


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
      .concat([root_split[MAX_S].match(/^\s*/)[0] + span(".hide", ["[...]"])])
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

  lines = lines.map(l => span(".lines", [" ".repeat(3 - l.toString().length) + l + " "]))

  var str = [
     pre.split("\n").map((s, i) => (i == 0 ? [] : ["br"]).concat([hide(s)])).reduce(flatten, []),
    root.split("\n").map((s, i) => (i == 0 ? [] : ["br"]).concat([show(s)])).reduce(flatten, []),
    post.split("\n").map((s, i) => (i == 0 ? [] : ["br"]).concat([hide(s)])).reduce(flatten, [])
  ].reduce(flatten, [])

  var split_str = str
  // if(split_str[split_str.length - 1].length == 0) split_str = split_str.slice(0, -1);
  str = split_str
    .reduce(({acc, i}, el) => {
      let isBr = el == "br";
      return {
        acc: acc.concat(isBr ? [br(), span(".linenr", [lines[i]])] : [el]),
        i: isBr ? i + 1 : i
      }
    }, {acc: [], i: 1})
    .acc;
  return span([lines[0]].concat(str));
}


module.exports = {
  getCodeStringFromPc,
  get_pc_to_inst_map,
  genSrcmapArr
};
