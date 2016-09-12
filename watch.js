var fs = require('fs');
var running = false;
fs.watch('src', function() {
  if (!running) {
    running = true;
    var spawn = require('child_process').spawn;
    var build = spawn('./node_modules/jengaboot/compile', {
      stdio: ['inherit', 'inherit', 'inherit']
    });
    build.on('close', function() {
      console.log("\n---------DONE---------\n");
      running = false;
    })
  }
});
