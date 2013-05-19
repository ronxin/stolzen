select count(*) from (
  select docid, sum(count) c from frequency group by docid having c > 300
);
