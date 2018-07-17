const clc = require('cli-color');

const hide_color = 244;
const show_color = 255;

const rightPad = (str, max) => {
  let bufferLength = max - clc.getStrippedLength(str || "").toString();
  if(bufferLength < 0) bufferLength = 0;
  return " ".repeat(bufferLength);
}

// TODO - default attrs is everything
const formatDb = function (db, attrs, style = () => false) {
  // get the desired attributes from the db objects
  let table = db.map(row => attrs.map(attr => row[attr] || ""));
  // Add attrs as titles
  table = ([attrs.map(t => t.toUpperCase())]).concat(table);

  let tableSizes = table
    .reduceRight((a, row) => row.map( (s,i) => Math.max(s && clc.getStrippedLength(s.toString()) || 0, a[i] || 0) ), []);

  let SPACE = 3;
  let formattedTable = table
    .map(e => e.slice(1).map((str, i) => str + rightPad(str, tableSizes[i + 1])).join(" ".repeat(SPACE)))
    .map((e, i) => table[i][0] + rightPad(table[i][0], tableSizes[0]) + " ".repeat(SPACE) + (i > 0 && (s = style(db[i - 1])) && clc.xterm(s)(e) || e))
    // .map(e => e.map((str, i) => str + rightPad(str, tableSizes[i])).join(" ".repeat(SPACE)))
    // .map((e, i) => i > 0 && (s = style(db[i - 1])) && clc.xterm(s)(e) || e)
    .join("\n");

  return formattedTable;

}

const formatCleanTree = function (tree, loc) {
  const fArray = (e, s) =>
    Array.isArray(e) && e.length > 1 ? e[0] + "\n" + e.slice(1).map(ee => s+ee).join("\n") : e

  const prefix = (row, dot = true) => "  " + (dot ? ".": " ").repeat(Math.max(0, max - row[0].length - 6)) + "  "
  let table = foldCleanPreorder(tree, loc)
  let max = table
    .reduceRight((a, row) => (row[0].length > a) ? row[0].length : a, 0);
  let str = table.map(row => row[0] + ((1 in row) && (prefix(row) + fArray(row[1], row[0].replace(/\w/g, " ") + prefix(row, false)) ) || "")).join("\n")
  return str;
}


const foldCleanPreorder = function (tree, loc, prefix = "", last = true) {
  let table = [];
  let __prefix = prefix + (last ? "╘ ": "╞ ")
  table.push([__prefix + clc.yellow(tree[loc])]);

  Object.keys(tree)
    .filter(name => name !== "children")
    .forEach(name => {
      if(name !== loc) {
        let str = prefix +  ((last || tree.children.length === 0) ? (tree.children.length === 0 && !last ? "│   " : "  │ ") : "│ ") + "" + name ;
        table.push([str, tree[name]]);
      }
    })

  let childs = tree.children
  let l = tree.children.length - 1;

  let rest = childs
  .map((child, i) => {
    let prefix_ = prefix + (last ? "  " : "│ ")
    let last_ = i === l;
    return foldCleanPreorder(child, loc, prefix_, last_)
  })
  .reverse()
  .reduceRight((a, c) => a.concat(c), [])

  return table.concat(rest)
}

const foldPreorder = function (_o) {
  let {
    tree,
    loc,
    style,
  } = {
    style: () => false,
    ..._o
  }

  let table = [ tree ]
  // TODO - compute total width - currently hardcoded to 100
  let ml = (tree.deltaC || "")
    .split("\n")
    .map(s =>
      s.match(new RegExp(`.{1,${ 100 }}`, "g"))
    )
    .reduce((a,e) => a.concat(e), [])
  if(ml.length > 1) {
    let deltaC = ml
    // console.log("e", tree.in_history, tree);
    table = deltaC
      .map((c, i) => {

        let ISOLD = tree.in_history;
        let IFULL = tree.children.length > 0;
        let ITAIL = i > 0;
        let ISTOP = i == 0;
        let ILAST = i == deltaC.length - 1;
        let ISACT = tree.branching;

        let smb = ITAIL && (
            IFULL && (
              ISOLD &&                   "┃ "
              || ISACT &&                "│ "
              || clc.xterm(hide_color)  ("│ ")
            )
            ||                           "  "
          )
          || tree[loc]

        return {
          ...tree,
          [loc]: smb,
          deltaC: (ISTOP ? "╭ " : (ILAST ? "╰ " : "│ ")) + c.trim()
        }
      })
  } else {
    tree.__node = true;
    tree.deltaC = (tree.deltaC || "").trim()
  }

  let wait = tree.children
    .map(c => c.in_history)
    .reduce((a, c) => a || c, false)
  let index = tree.children
    .reduce((a, e, i) => (
      (a > -1 || !e.in_history)
      ?  a
      : i
    ), -1)
  let rest = tree.children
  .map((child, i) => {
    let last_ = i === tree.children.length - 1;
    return foldPreorder({
        tree: child,
        loc,
        style
      })
      .map((e, j) => {

        let INODE = j == 0;
        let ILAST = last_;
        let ISOLD = e.in_history;
        let IWAIT = wait && i < index;
        let ISACT = e.branching;
        let IPACT = child.branching;

        let smb =
          INODE && (
            ILAST && (
              ISOLD &&                           "┗ "
              || ISACT && clc.xterm(show_color) ("╘ ")
              || clc.xterm(hide_color)          ("└ ")
            )
            || ISOLD &&                          "┗ "
            || IWAIT && clc.xterm(show_color)   ("┃ ")
            || ISACT && clc.xterm(show_color)   ("╞ ")
            || clc.xterm(hide_color)            ("├ ")
          )
          || ILAST &&                            "  "
          || IWAIT && clc.xterm(show_color)     ("┃ ")
          || IPACT &&                            "│ "
          || clc.xterm(hide_color)              ("│ ")


        e[loc] = smb + e[loc];
        return e;
      })
  })
  .reduce((a, c) => a.concat(c), [])

  return table.concat(rest)
}



module.exports = {
  formatCleanTree,
  formatDb,
  foldCleanPreorder,
  foldPreorder
}
