unit wcdebug_vars;

{$mode objfpc}{$H+}

interface

var
  DEBUG_GLOBALS_LONGWORD : Array [0..13] of Cardinal = (0, 0, 0,
                                                       0, 0, 0,
                                                       0, 0, 0,
                                                       0, 0, 0,
                                                       0, 0);

  DEBUG_GLOBALS_QWORD    : Array [0..11] of QWORD   = (0, 0, 0,
                                                       0, 0, 0,
                                                       0, 0, 0,
                                                       0, 0, 0);

const DG_MAX_LIVE_LINEAR_JOBS = 0;
const DG_MAX_LIVE_SORTED_JOBS = 1;
const DG_COUNT_LINEAR_JOBS = 2;
const DG_COUNT_SORTED_JOBS = 3;
const DG_WAIT_DELTA_TIME_MAX = 4;
const DG_WAIT_DELTA_TIME_AV = 5;
const DG_READ_DELTA_TIME_MAX = 6;
const DG_READ_DELTA_TIME_AV = 7;
const DG_WORK_DELTA_TIME_MAX = 8;
const DG_WORK_DELTA_TIME_AV = 9;
const DG_WRITE_DELTA_TIME_MAX = 10;
const DG_WRITE_DELTA_TIME_AV = 11;
const DG_FAILED_PREP_CNT = 12;
const DG_MAX_CONCURRENT_STREAMS = 13;

const DG_WAIT_DELTA_TIME_SUM = 0;
const DG_WAIT_DELTA_TIME_CNT = 1;
const DG_READ_DELTA_TIME_SUM = 2;
const DG_READ_DELTA_TIME_CNT = 3;
const DG_WORK_DELTA_TIME_SUM = 4;
const DG_WORK_DELTA_TIME_CNT = 5;
const DG_WRITE_DELTA_TIME_SUM = 6;
const DG_WRITE_DELTA_TIME_CNT = 7;

implementation

end.

