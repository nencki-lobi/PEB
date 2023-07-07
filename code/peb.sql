\f '|'
\pset footer


-- Conditions

\o ./data/conditions.tsv

SELECT s.sid, s.stid,
    SUBSTRING (srt.code FOR 3) AS category -- presentation screen (ANG, COM, HOP, NEU)
FROM subject s
JOIN qcopy q ON q.rid = s.sid
JOIN srt ON srt.qid = q.qid
JOIN recruitment r ON r.sid = s.sid
WHERE s.stid IN (23)
    AND r.status = 0
    AND NOT srt.code = 'eval' -- evaluation screen
GROUP BY s.sid, category
ORDER BY s.stid, s.sid;

-- Demographic

\o ./data/demographic.tsv

SELECT s.sid, s.stid, q.name,
    a.ord, a.val
FROM subject s
JOIN qcopy q ON q.rid = s.sid
JOIN answer a ON a.qid = q.qid
JOIN recruitment r ON r.sid = s.sid
WHERE s.stid IN (23)
    AND r.status = 0
    AND q.name = 'demo-2-pl'
ORDER BY s.stid, s.sid, a.ord;

-- Questionnaires

\o ./data/questionnaires.tsv

SELECT s.sid, s.stid, q.name,
    c.ord, c.opt
FROM subject s
JOIN qcopy q ON q.rid = s.sid
JOIN choice c ON c.qid = q.qid
JOIN recruitment r ON r.sid = s.sid
WHERE s.stid IN (23)
    AND r.status = 0
    AND q.name IN ('PCAE-pl', 'PD-pl', 'WTS-pl')
ORDER BY s.stid, s.sid, q.name, c.ord;

-- Story ratings

\o ./data/story-ratings.tsv

SELECT s.sid, s.stid, q.name,
    b.ord, b.part, b.opt
FROM subject s
JOIN qcopy q ON q.rid = s.sid
JOIN bchoice b ON b.qid = q.qid
JOIN recruitment r ON r.sid = s.sid
WHERE s.stid IN (23)
    AND r.status = 0
    AND q.name ='rateus-pl'
ORDER BY s.stid, s.sid, b.part;

-- Donation intentions

\o ./data/peb-intentions.tsv

SELECT s.sid, s.stid, q.name,
    c.ord, c.opt
FROM subject s
JOIN qcopy q ON q.rid = s.sid
JOIN choice c ON c.qid = q.qid
JOIN recruitment r ON r.sid = s.sid
WHERE s.stid IN (23)
    AND r.status = 0
    AND q.name = 'donation-aim-pl'
ORDER BY s.stid, s.sid, c.ord;

-- WEPT

\o ./data/peb-weptings.tsv

SELECT s.sid, s.stid, q.name,
    w.ord, w.mh, w.fh
FROM subject s
JOIN qcopy q ON q.rid = s.sid
JOIN weptitem w ON w.qid = q.qid
JOIN recruitment r ON r.sid = s.sid
WHERE s.stid IN (23)
    AND r.status = 0
    AND q.name IN ('wept-pl')
ORDER BY s.stid, s.sid, w.ord;

-- Donation

\o ./data/peb-donations.tsv

SELECT s.sid, s.stid, q.name,
    t.ord, t.val
FROM subject s
JOIN qcopy q ON q.rid = s.sid
JOIN txt t ON t.qid = q.qid
JOIN recruitment r ON r.sid = s.sid
WHERE s.stid IN (23)
    AND r.status = 0
    AND q.name = 'wept-pl'
ORDER BY s.stid, s.sid;

\o ./data/rating_times.tsv

SELECT s.sid, s.stid, q.name, AVG(srt.time) AS time
FROM subject s
JOIN qcopy q ON q.rid = s.sid
JOIN srt ON srt.qid = q.qid
JOIN recruitment r ON r.sid = s.sid
WHERE s.stid IN (23)
    AND r.status = 0
    AND NOT srt.code = 'eval'
GROUP BY s.sid, q.name
ORDER BY s.sid;
