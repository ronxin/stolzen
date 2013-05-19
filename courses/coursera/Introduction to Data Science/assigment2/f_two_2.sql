select count(*) from 
  (select docid from frequency where term='transaction') s1 
  join 
  (select docid from frequency where term='world') s2
  on s1.docid = s2.docid; 
