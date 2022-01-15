"use strict";

exports.confettiImpl = function(opts) {
  return function() {
    confetti(opts);
  }
}
