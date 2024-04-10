\pset footer

-- Create temporary view for later reuse

CREATE TEMP VIEW valid AS
    (
    SELECT s.sid, s.code, s.stid
    FROM subject s
    LEFT JOIN recruitment r ON r.sid = s.sid
    WHERE s.stid = 23 
        AND r.status = 0
    )

    UNION ALL

    (
    SELECT s.sid, s.code, s.stid
    FROM subject s
    LEFT JOIN external_kantar e ON e.sid = s.sid
    WHERE s.stid = 26 
        AND (e.kantar_stid = 1 AND e.problems = 0)
    )

    ORDER BY stid, sid;

-- Conditions

\o ./data/conditions.csv

SELECT s.sid, s.stid,
    SUBSTRING (srt.code FOR 3) AS category -- presentation screen (ANG, COM, HOP, NEU)
FROM valid s
JOIN qcopy q ON q.rid = s.sid
JOIN srt ON srt.qid = q.qid
WHERE NOT srt.code = 'eval' -- evaluation screen
GROUP BY s.stid, s.sid, category
ORDER BY s.stid, s.sid;

-- Demographic

\o ./data/demographic.csv

SELECT s.sid, s.stid, q.name,
    a.ord, a.val
FROM valid s
JOIN qcopy q ON q.rid = s.sid
JOIN answer a ON a.qid = q.qid
WHERE q.name ~ '^demo'
ORDER BY s.stid, s.sid, a.ord;

-- Questionnaires

\o ./data/questionnaires.csv

SELECT s.sid, s.stid, q.name,
    c.ord, c.opt
FROM valid s
JOIN qcopy q ON q.rid = s.sid
JOIN choice c ON c.qid = q.qid
WHERE q.name ~ '^(PCAE|PD|WTS)'
ORDER BY s.stid, s.sid, q.name, c.ord;

-- Story ratings

\o ./data/story-ratings.csv

SELECT s.sid, s.stid, q.name,
    b.ord, b.part, b.opt
FROM valid s
JOIN qcopy q ON q.rid = s.sid
JOIN bchoice b ON b.qid = q.qid
WHERE q.name ~ '^rateus'
ORDER BY s.stid, s.sid, b.part;

-- Story reading & evaluation time

\o ./data/story-times.csv

WITH 
rtime AS
    (SELECT qid, AVG(time)::numeric(10,2) AS reading_time
    FROM srt WHERE NOT code = 'eval'
    GROUP BY qid),
etime AS
    (SELECT qid, time AS evaluation_time
    FROM srt WHERE code = 'eval')
SELECT s.sid, s.stid, q.name, 
    reading_time, evaluation_time 
FROM rtime
JOIN etime ON etime.qid = rtime.qid
JOIN qcopy q ON q.qid = rtime.qid
JOIN valid s ON s.sid = q.rid
ORDER BY s.stid, s.sid;

-- Donation intentions

\o ./data/peb-intentions.csv

SELECT s.sid, s.stid, q.name,
    c.ord, c.opt
FROM valid s
JOIN qcopy q ON q.rid = s.sid
JOIN choice c ON c.qid = q.qid
WHERE q.name ~ '^donation-aim'
ORDER BY s.stid, s.sid, c.ord;

-- WEPT

\o ./data/peb-weptings.csv

SELECT s.sid, s.stid, q.name,
    w.ord, w.mh, w.fh
FROM valid s
JOIN qcopy q ON q.rid = s.sid
JOIN weptitem w ON w.qid = q.qid
WHERE q.name ~ '^wept'
ORDER BY s.stid, s.sid, w.ord;

-- Donation

\o ./data/peb-donations.csv

SELECT s.sid, s.stid, q.name,
    t.ord, t.val
FROM valid s
JOIN qcopy q ON q.rid = s.sid
JOIN txt t ON t.qid = q.qid
WHERE q.name ~ '^wept'
ORDER BY s.stid, s.sid;
