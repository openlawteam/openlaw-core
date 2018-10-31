const path = require('path');
const webpack = require('webpack');

// https://webpack.js.org/plugins/dll-plugin/#dllreferenceplugin
module.exports = {
  resolve: {
    extensions: ['.js', '.jsx', '.json', '.less', '.css'],
    modules: [__dirname, 'node_modules'],
  },
  entry: {
    'openlawVendorDll': [
      'axios',
      './client/build/Openlaw.bundle.js',
      'react',
      'react-dom',
      'react-redux',
      'redux',
    ],
  },
  output: {
    filename: '[name].bundle.js',
    path: path.join(__dirname, 'client/build/vendor'),
    library: '[name]'
  },
  plugins: [
    new webpack.DllPlugin({
      context: __dirname,
      name: '[name]',
      path: path.join(__dirname, 'client/build/vendor', '[name].json'),
    })
  ],
};
