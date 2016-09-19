var path = require("path");
var webpack = require("webpack");

module.exports = {
  cache: true,
  devtool: "cheap-module-eval-source-map",
  entry: {
    app: path.join(__dirname, "_build", "src", "client.js")
  },
  output: {
    path: path.join(__dirname, "_build", "src"),
    filename: "app.js",
  },
  resolve: {
    extensions: ["", ".js", ".jsx"],
    root: path.resolve(__dirname, "_build"),
    modulesDirectories: ["node_modules"]
  }
};
