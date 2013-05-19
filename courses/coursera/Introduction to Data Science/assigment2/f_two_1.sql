select count(*) from (
  select distinct docid from frequency where term='transaction'
  intersect
  select distinct docid from frequency where term='world'
);

