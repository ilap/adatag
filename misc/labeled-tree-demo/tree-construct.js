function fromList(values) {
  var length = values.length;
  function go (idx) {
    if (idx > length) {
      return
    } else {
      var v = values[idx - 1]
      var lnode = go (2 * idx)
      var rnode = go (2 * idx + 1)
      
      v.leftNode = lnode;
      v.rightNode = rnode;
      if (idx == 1) {
        v.isRoot = true
      }

      return v;
    }
  }
  var tree = go (1);
  //tree.hash = tree.toHexString(tree.proof());
  tree.proof();
  return tree
}

function convertToNodeList(dataChunkList) {
  var nodeList = [];

  for (var i in dataChunkList) {
    nodeList.push(new Node(dataChunkList[i][0], dataChunkList[i][1]));
  }
  return nodeList;
}


function Node (a, b, hash, leftNode, rightNode) {
    this.hash = hash;
    this.a = a
    this.b = b
    this.val = a + b;
    this.leftNode = leftNode;
    this.rightNode = rightNode;
    this.isRoot = false;
    this.emptyHash = sha256.update("").array()
    
    this.toHexString = function (arr) {
      var array = new Uint8Array(arr);
      var hex = '';
      for (var i = 0; i < array.length; ++i) {
        var c = array[i].toString('16');
        hex += c.length === 1 ? '0' + c : c;
      }
      return hex;
    }
    this.proof = function () {
      var lp = this.leftNode  === undefined ? this.emptyHash : this.leftNode.proof();
      var rp = this.rightNode === undefined ? this.emptyHash : this.rightNode.proof();
      var hv = sha256.update(this.val).array();
      var h = hv.concat(lp.concat(rp));
      var p = sha256.update(h).array();
      
      this.hash = this.toHexString(p);
      return p
    }

    this.toString = function() {
      if(this.leftNode || this.rightNode) {
        var returnObj = {
          text: {
            datav: "val    : " + this.a + ", "+ this.b,
            datah: "proof n: " + this.hash.substr(0, 6),
            // datal: "proof l: " + this.proof(this.left).substr(0, 6),
            // datar: "proof r: " + this.proof(this.right).substr(0, 6),
          },
          HTMLclass: this.isRoot ? "root-node" : "parent-node",
          children: []
        };

        if (this.leftNode) returnObj.children.push(this.leftNode.toString());
        if (this.rightNode) returnObj.children.push(this.rightNode.toString());
        return returnObj;
      }
      return {
        text: {
          datav: "val  : " + this.a + ", "+ this.b,
          datah: "proof: " + this.hash.substr(0, 6),
        },
        HTMLclass: "label-node"
      };
    }
}

///////////////////Test/////////////////////////////////////////////////////////
// var testData = ['a','b','c','d','e','f','g','h'];
// var nodeList = convertToNodeList(testData);
// console.log(JSON.stringify(constructTree(nodeList)[0].toString()));
/*
const sha256 = require('js-sha256');

var testData = [["`", "c"], ["z", "{"], ["f", "g"], ["j", "l"], ["d", "e"], ["y", "z"], ["g", "h"], ["l", "o"], ["u", "v"], ["s", "u"], ["x", "y"], ["e", "f"], ["o", "s"], ["i", "j"], ["v", "x"], ["c", "d"], ["h", "i"]];

var nodeList = convertToNodeList(testData);
console.log(JSON.stringify(fromList(nodeList).toString()));

*/