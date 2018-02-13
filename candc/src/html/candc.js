
function tog(id){
  var e = document.getElementById(id);
  var children = e.parentNode.firstChild;
  if(children.style.display == "none"){
    children.style.display = "block";
  }else{
    children.style.display = "none";
  }
};

function apply(node, colour){
  for(var i = 0; i < node.childNodes.length; ++i){
    var e = node.childNodes[i];
    if(e.style){
      e.style.color = colour;
    }
    apply(e, colour);
  }
};

function mo(id){
  apply(document.getElementById(id).parentNode, "#f00");
};

function mu(id){
  apply(document.getElementById(id).parentNode, "#000");
};
