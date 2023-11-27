function initConstructTree(){
  var testData = [["`", "c"], ["z", "{"], ["f", "g"], ["j", "l"], ["d", "e"], ["y", "z"], ["g", "h"], ["l", "o"], ["u", "v"], ["s", "u"], ["x", "y"], ["e", "f"], ["o", "s"], ["i", "j"], ["v", "x"], ["c", "d"], ["h", "i"]];
 
  var nodeList = convertToNodeList(testData);
 
  var nodeTreeStructure = fromList(nodeList).toString();
 
  var chart_config = {
    	chart: {
    		container: "#labeled-tree",
        connectors: {
          "type":"step",
          "style": {
            "stroke-width": 2
          }
        }
    	},
    	nodeStructure: nodeTreeStructure
  };
  new Treant( chart_config );
}