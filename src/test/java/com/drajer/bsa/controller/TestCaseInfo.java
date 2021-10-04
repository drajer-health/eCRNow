package com.drajer.bsa.controller;

public class TestCaseInfo {

  private String planDef;
  private String name;
  private boolean shouldTrigger;
  private Integer initialPopulation;
  private Integer denominator;
  private Integer denomExclusion;
  private Integer numerator;

  public TestCaseInfo(String planDef,
  String name,
  boolean shouldTrigger)
  {
    this(planDef, name, shouldTrigger, null, null, null, null);
  }

  public TestCaseInfo(String planDef,
  String name,
  boolean shouldTrigger,
  Integer initialPopulation)
  {
    this(planDef, name, shouldTrigger, initialPopulation, null, null, null);
  }

  public TestCaseInfo(
      String planDef,
      String name,
      boolean shouldTrigger,
      Integer initialPopulation,
      Integer denominator,
      Integer denomExclusion,
      Integer numerator) {
    this.planDef = planDef;
    this.name = name;
    this.shouldTrigger = shouldTrigger;
    this.initialPopulation = initialPopulation;
    this.denominator = denominator;
    this.denomExclusion = denomExclusion;
    this.numerator = numerator;
  }

  public String getPlanDef() {
    return this.planDef;
  }

  public String getName() {
    return this.name;
  }

  public boolean getShouldTrigger() {
    return this.shouldTrigger;
  }

  public Integer getInitialPopulation() {
    return this.initialPopulation;
  }

  public Integer getDenominator() {
    return this.denominator;
  }

  public Integer getDenominatorExclusion() {
    return this.denomExclusion;
  }

  public Integer getNumerator() {
    return this.numerator;
  }

  @Override
  public String toString() {
    return this.getPlanDef() + "/" + this.getName();
  }
}
