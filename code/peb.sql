\f '|'
\pset footer


-- Conditions

\o ./conditions.tsv

SELECT srt.qid, s.sid, s.stid,
       CASE WHEN srt.code LIKE 'ANG%' THEN 'ANG'
            WHEN srt.code LIKE 'COM%' THEN 'COM'
            WHEN srt.code LIKE 'HOP%' THEN 'HOP'
            WHEN srt.code LIKE 'NEU%' THEN 'NEU'
            ELSE 'other'
       END AS category
FROM srt
JOIN qcopy q ON q.qid = srt.qid
JOIN subject s ON q.rid = s.sid
WHERE s.stid IN (23)
GROUP BY srt.qid, s.sid, category;

-- WEPT

\o ./peb-weptings.tsv

SELECT s.sid, s.stid,
       w.ord, w.mh, w.fh,
       q.name
FROM subject s
JOIN qcopy q ON q.rid = s.sid
JOIN weptitem w ON w.qid = q.qid
WHERE s.stid IN (23)
  AND q.name IN ('wept-pl')
ORDER BY s.stid, s.sid;

-- Donation

\o ./peb-donations.tsv

SELECT 
s.sid, s.stid,
q.name, a.ord, a.val
FROM subject s
JOIN qcopy q ON q.rid = s.sid
JOIN answer a ON a.qid = q.qid
WHERE s.stid IN (23)
 AND q.name = 'wept-pl'
ORDER BY s.stid, s.sid, a.ord;

-- Questionnaires

\o ./questionnaires.tsv

SELECT 
s.sid, s.stid,
q.name,
c.ord, c.opt
FROM subject s
JOIN qcopy q ON q.rid = s.sid
JOIN choice c ON c.qid = q.qid
WHERE s.stid IN (23)
 AND q.name IN ('PCAE-pl', 'PD-pl', 'WTS-pl')
ORDER BY s.sid;

-- Donation Intentions

\o ./peb-intentions.tsv

SELECT 
s.sid, s.stid,
q.name,
c.ord, c.opt
FROM subject s
JOIN qcopy q ON q.rid = s.sid
JOIN choice c ON c.qid = q.qid
WHERE s.stid IN (23)
 AND q.name ='donation-aim-pl'
ORDER BY s.sid;

-- Story ratings

\o ./story-ratings.tsv

SELECT 
s.sid, s.stid,
q.name, 
b.ord, b.part, b.opt
FROM subject s
JOIN qcopy q ON q.rid = s.sid
JOIN bchoice b ON b.qid = q.qid
WHERE s.stid IN (23)
 AND q.name ='rateus-pl'
ORDER BY s.sid;

-- Demographic

\o ./demographic.tsv

SELECT 
s.sid, s.code, s.stid,
q.name,
a.ord, a.val
FROM subject s
JOIN qcopy q ON q.rid = s.sid
JOIN answer a ON a.qid = q.qid
WHERE s.stid IN (23)
 AND q.name = 'demo-2-pl'
ORDER BY s.sid;

-- Condition + WEPT

\o ./peb-cond-weptings.tsv

WITH condition AS (
SELECT srt.qid, s.sid,
       CASE WHEN srt.code LIKE 'ANG%' THEN 'ANG'
            WHEN srt.code LIKE 'COM%' THEN 'COM'
            WHEN srt.code LIKE 'HOP%' THEN 'HOP'
            WHEN srt.code LIKE 'NEU%' THEN 'NEU'
            ELSE 'other'
       END AS category
FROM srt
JOIN qcopy q ON q.qid = srt.qid
JOIN subject s ON q.rid = s.sid
GROUP BY srt.qid, s.sid, category
),
weptings AS (
SELECT weptitem.qid, s.sid, weptitem.ord, mh,fh
FROM weptitem
JOIN qcopy q ON q.qid = weptitem.qid
JOIN subject s ON q.rid = s.sid
)
SELECT c.sid, s.stid, c.category, w.ord, w.mh, w.fh
FROM condition c
JOIN weptings w ON c.sid = w.sid
JOIN subject s ON c.sid = s.sid
--JOIN answer a ON s.sid = a.sid
WHERE s.stid = 23 AND c.category <> 'other'
ORDER BY c.category;

