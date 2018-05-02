const clc = require('cli-color');
const jsondiffpatch = require("jsondiffpatch").create({
   textDiff: {
        // default 60, minimum string length (left and right sides) to use text diff algorythm: google-diff-match-patch
        minLength: 10
    }
});

const toArray = (json, diff, o = {indent: 0}) => {
  switch(typeof json) {
    case "object":
      let keys = Object.keys(json)
        .map(key => toArray(json[key], diff && key in diff && diff[key], {indent: o.indent + 1, key}))
        .reduce((a, e) => a.concat(e), []);
      return "key" in o
              ? [Object.assign(o)].concat(keys)
              : keys
      break;
    case "string":
      let o_ = Object.assign({}, o)
      if(diff && diff.length == 2) {
        // o_.value = clc.red(diff[0]) + "\n" + clc.green(diff[1])
        o_.value = clc.yellow(diff[1])
      } else if(diff && diff.length == 3) {
        // console.log(diff[0]);
        // console.log(require("jsondiffpatch").formatters.console.format(diff));
        // let mp = diff[0]
        let str = clc.yellow(json);
        // for(let i=0; i < mp.length; i+=3) {
        //   // let [bla, a, b, c, d] = mp[i].match(/\@\@\s(-\d+),(\d+)\s\+(\d+),(\d+)\s\@\@/);
        //   // str = str.slice(0, a) + mp[i + 1] + str.slice(b);
        // }
        o_.value = str;
      } else {
        o_.value = json;
      }

      o_.value = o_.value.split("~>").join("\n~>");
      // o_.value = o_.value.split(":").join(":\n");
      if(o_.value.split("\n").length > 1) {
        let sp = o_.value.split("\n");
        o_.value = sp[0]
        return [o_].concat(sp.slice(1).map(val => ({value: val.trim()})))
      }
      return [o_];
      break;
    default:
      console.log('ERROR');
  }
}

const prepare = arr => {
  return arr.map(o => {
    return ["  ".repeat(o.indent - 1)+  (o.key === "pc" ? clc.blue(o.key) : o.key || ""), o.value, o.diff];
  })
}

const formatTable = arr => {
  let maxInt = [];
  arr = [["KEY", "VALUE", ""]].concat(arr)
  arr[0].forEach(e => {maxInt.push(0)});
  arr.forEach(row => {
    row.forEach((e, i) => {
      if(maxInt[i] < clc.getStrippedLength(e || "")) maxInt[i] = clc.getStrippedLength(e)
    })
  });
  return arr.map(row => row.map((cell, i) => {
    return (cell || "") +  " ".repeat(maxInt[i] - clc.getStrippedLength(cell || "") + 2)
  }).join("")).join("\n");
}

const formatStep = (t, s) => {
  var diff = {};
  if(s) {
    diff = jsondiffpatch.diff(s, t);
  }
  return formatTable(prepare(toArray(t, diff)));
}

module.exports = {
  formatStep
}
