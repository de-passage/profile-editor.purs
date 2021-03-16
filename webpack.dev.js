const merge = require('webpack-merge');
const common = require('./webpack.common.js');

module.exports = merge.smart(common, {
    mode: 'development',

    devServer: {
        contentBase: '.',
        port: 8080,
        stats: 'errors-only'
    },
});