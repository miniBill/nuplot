<?xml version="1.0" encoding="UTF-8" ?>
<!DOCTYPE Workset SYSTEM "workset.dtd" >
<Workset version="1">
  <WorksetName>CDA</WorksetName>
  <Description>This workset contains all JAR files that comprise the 
Class Dependency Analyzer. 
It can be used as a starting point to get familiar with
this tool.</Description>
  <Classpath>
    <ClasspathPart>{CDA_HOME}/pf-cda*.jar</ClasspathPart>
    <ClasspathPart>{CDA_HOME}/lib/*.jar</ClasspathPart>
  </Classpath>
  <ViewFilters>
    <PatternFilter active="yes">java.*</PatternFilter>
    <PatternFilter active="yes">javax.*</PatternFilter>
    <PatternFilter active="yes">com.sun.*</PatternFilter>
  </ViewFilters>
  <IgnoreFilters>
    <PatternFilter active="yes">java.*</PatternFilter>
    <PatternFilter active="yes">javax.*</PatternFilter>
    <PatternFilter active="yes">com.sun.*</PatternFilter>
  </IgnoreFilters>
</Workset>