"use strict";

exports["playSound'"] = function playSound(str) {
  return function() {
    document.getElementById(str + "-audio").play();
    return {};
  }
}

