var HOURS = 0;
var MINUTES = 1;
var SECONDS = 2;
var SUBSECONDS = 3;
var DATE = 4;
var SEASON = 5;

var CREATION = Date.UTC(2015, 10, 29, 23, 34, 14);
var WINGFLAP_MILLIS  = 4;
var SUBSECOND_MILLIS = WINGFLAP_MILLIS * 36;
var SECOND_MILLIS    = SUBSECOND_MILLIS * 36;
var MINUTE_MILLIS    = SECOND_MILLIS * 36;
var HOUR_MILLIS      = MINUTE_MILLIS * 36;
var DAY_MILLIS       = HOUR_MILLIS * 10;
var WEEK_MILLIS      = DAY_MILLIS * 6;
var MONTH_MILLIS     = DAY_MILLIS * 30;
var YEAR_MILLIS      = DAY_MILLIS * 360;

var mythCycle = [
  ['Divolm', 'Thunder'],
  ['Telzlnoln', 'Rain'],
  ['Jidolk', 'Flower'],
  ['Shelsheln', 'River'],
  ['Thefam', 'Stone'],
  ['Zatheln', 'Spider'],
  ['Kizhult', 'Bee'],
  ['Thefnolm', 'Bear'],
  ['Vithit', 'Bird']
];

var letterNames = [
  ['Duhdem', 'Dam', 'd'],
  ['Gigim', 'Flipper', 'g'],
  ['Xataxym', 'Pit', 'x'],
  ['Jegen', 'Hook', 'j'],
  ['Fijyc', 'Rainbow', 'f'],
  ['Voljam', 'Ear', 'v'],
  ['Thethat', 'Wind', 'th'],
  ['Sekelt', 'Valley', 's'],
  ['Zuhzuhmelt', 'Ladle', 's'],
  ['Shuhzhik', 'Tear', 'sh'],
  ['Zhizlik', 'Fish', 'zh'],
  ['Slik', 'Thumbs-up', 'sl'],
  ['Zlolfit', 'Wing', 'zl'],
  ['Molmelc', 'Roof', 'm'],
  ['Nyzlan', 'Snail', 'n'],
  ['Nasham', 'Wave', 'a'],
  ['Xelteln', 'Cliff', 'el'],
  ['Tezet', 'Lightning', 'e'],
  ['Tolmolm', 'Slope', 'ol'],
  ['Mizizlat', 'Cart', 'i'],
  ['Slysyc', 'Snake', 'y'],
  ['Shnuhk', 'Lips', 'uh'],
  ['Tuln', 'Eye', 'ul'],
  ['Cuhc', 'Foot', 'c'],
  ['Tytyt', 'Clover', 't'],
  ['Kyfik', 'Arm', 'k'],
  ['Zlnanic', 'Chameleon', 'ah'],
  ['Thnuhduhk', 'Elephant', 'eh'],
  ['Snolzem', 'Knot', 'o'],
  ['Vmyn', 'Mouth', 'u']
];

var seasons = ['Egg', 'Larva', 'Pupa', 'Worker', 'Drone', 'Queen'];

var textualDisplay = [];

function $(id) {
  return document.getElementById(id);
}

function setup() {
  textualDisplay = [
    $('textual-hours'),
    $('textual-minutes'),
    $('textual-seconds'),
    $('textual-subseconds'),
    $('textual-date'),
    $('textual-season')
  ];
  setInterval(displayNow, SUBSECOND_MILLIS);
}

function displayNow() {
  displayDate(new Date());
}

function displayDate(date) {
  var honeyDate = gregorianToHoney(date);
  setTextualDisplay(honeyDate);
}

function setTextualDisplay(honeyDate) {
  textualDisplay[HOURS].innerHTML = honeyDate.hours;
  textualDisplay[MINUTES].innerHTML = toSenary(honeyDate.minutes, 2);
  textualDisplay[SECONDS].innerHTML = toSenary(honeyDate.seconds, 2);
  textualDisplay[SUBSECONDS].innerHTML = toSenary(honeyDate.subseconds, 2);
  textualDisplay[DATE].innerHTML = honeyDate.year + ' ' +
                                   mythCycle[honeyDate.mythRole][0] + ' ' +
                                   honeyDate.mythNumber + ', ' +
                                   honeyDate.month + ' ' +
                                   letterNames[honeyDate.dayOfMonth][0];
  textualDisplay[SEASON].innerHTML = seasons[honeyDate.season] + " Season";
}

function toSenary(value, width) {
  result = '';
  var mult;
  for (var i = width; i > 0; --i) {
    placeValue = Math.pow(6, i-1);
    result += Math.floor(value / placeValue);
    value %= placeValue;
  }
  return result;
}

function gregorianToHoney(date) {
  var millis = date.valueOf() - CREATION;
  var result = {
    year:       Math.floor(millis / YEAR_MILLIS),
    month:      Math.floor(millis % YEAR_MILLIS / MONTH_MILLIS),
    dayOfYear:  Math.floor(millis % YEAR_MILLIS / DAY_MILLIS),
    dayOfMonth: Math.floor(millis % MONTH_MILLIS / DAY_MILLIS),
    hours:      Math.floor(millis % DAY_MILLIS / HOUR_MILLIS),
    minutes:    Math.floor(millis % HOUR_MILLIS / MINUTE_MILLIS),
    seconds:    Math.floor(millis % MINUTE_MILLIS / SECOND_MILLIS),
    subseconds: Math.floor(millis % SECOND_MILLIS / SUBSECOND_MILLIS),
  };
  result.mythRole = result.dayOfYear % 9;
  result.mythNumber = result.dayOfYear % 40;
  result.season = Math.floor(result.month / 2);
  return result;
}
