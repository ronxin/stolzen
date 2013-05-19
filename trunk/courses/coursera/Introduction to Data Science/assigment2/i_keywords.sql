drop view if exists query;

create view query as 
SELECT 'q' as docid, 'washington' as term, 1 as count 
UNION
SELECT 'q' as docid, 'taxes' as term, 1 as count
UNION 
SELECT 'q' as docid, 'treasury' as term, 1 as count;

select max(count) from (
  select a.docid, b.docid, sum(a.count * b.count) as count 
  from frequency a join query b on a.term = b.term 
  group by a.docid, b.docid
);
