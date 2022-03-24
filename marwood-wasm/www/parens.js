// Copyright (c) 2018-2021 Jakub T. Jankiewicz
// This file contains source from the jquery terminal examples and JS Lips project,
// MIT licensed and available at: https://github.com/jcubic/lips

var pre_parse_re =
  /("(?:\\[\S\s]|[^"])*"|\/(?! )[^\/\\]*(?:\\[\S\s][^\/\\]*)*\/[gimy]*(?=\s|\(|\)|$)|;.*)/g;
var tokens_re =
  /("(?:\\[\S\s]|[^"])*"|\/(?! )[^\/\\]*(?:\\[\S\s][^\/\\]*)*\/[gimy]*(?=\s|\(|\)|$)|\(|\)|'|"(?:\\[\S\s]|[^"])+|(?:\\[\S\s]|[^"])*"|;.*|(?:[-+]?(?:(?:\.[0-9]+|[0-9]+\.[0-9]+)(?:[eE][-+]?[0-9]+)?)|[0-9]+\.)[0-9]|\.|,@|,|`|[^(\s)]+)/gim;

var position;
var timer;

function tokenize(str) {
  var count = 0;
  var offset = 0;
  var tokens = [];
  str
    .split(pre_parse_re)
    .filter(Boolean)
    .forEach(function (string) {
      if (string.match(pre_parse_re)) {
        if (!string.match(/^;/)) {
          var col = (string.split(/\n/), [""]).pop().length;
          tokens.push({
            token: string,
            col,
            offset: count + offset,
            line: offset,
          });
          count += string.length;
        }
        offset += (string.match("\n") || []).length;
        return;
      }
      string
        .split("\n")
        .filter(Boolean)
        .forEach(function (line, i) {
          var col = 0;
          line
            .split(tokens_re)
            .filter(Boolean)
            .forEach(function (token) {
              var line = i + offset;
              var result = {
                col,
                line,
                token,
                offset: count + line,
              };
              col += token.length;
              count += token.length;
              tokens.push(result);
            });
        });
    });
  return tokens;
}

exports.set_position = function (term) {
  if (position) {
    term.set_position(position);
    position = false;
  }
};

exports.paren_match = function (term, e) {
  if (e.key == ")") {
    setTimeout(function () {
      position = term.get_position();
      var command = term.before_cursor();
      var len = command.split(/\n/)[0].length;
      var tokens = tokenize(command);
      var count = 1;
      var token;
      var i = tokens.length - 1;
      while (count > 0) {
        token = tokens[--i];
        if (!token) {
          return;
        }
        if (token.token === "(") {
          count--;
        } else if (token.token == ")") {
          count++;
        }
      }
      if (token.token == "(") {
        clearTimeout(timer);
        setTimeout(function () {
          term.set_position(token.offset);
          timer = setTimeout(function () {
            term.set_position(position);
            position = false;
          }, 200);
        }, 0);
      }
    }, 0);
  } else {
    position = false;
  }
};
