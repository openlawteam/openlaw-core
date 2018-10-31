let path = require('path');

module.exports = {
  entry: {
    Openlaw: path.resolve(__dirname, './client/target/scala-2.12/client.js'),
  },
  output: {
    filename: '[name].bundle.js',
    path: path.resolve(__dirname, './client/build'),
    libraryTarget: 'umd'
  }
};
