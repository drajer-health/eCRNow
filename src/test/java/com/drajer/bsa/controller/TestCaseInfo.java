package com.drajer.bsa.controller;

public class TestCaseInfo {

  private String planDefUrl;
  private String planDef;
  private String name;
  private ExpectedOutcome expectedOutcome;
  private Integer initialPopulation;
  private Integer denominator;
  private Integer denomExclusion;
  private Integer numerator;

  public TestCaseInfo(
      String planDef, String planDefUrl, String name, ExpectedOutcome expectedOutcome) {
    this(planDef, planDefUrl, name, expectedOutcome, null, null, null, null);
  }

  public TestCaseInfo(
      String planDef,
      String planDefUrl,
      String name,
      ExpectedOutcome expectedOutcome,
      Integer initialPopulation) {
    this(planDef, planDefUrl, name, expectedOutcome, initialPopulation, null, null, null);
  }

  public TestCaseInfo(
      String planDef,
      String planDefUrl,
      String name,
      ExpectedOutcome expectedOutcome,
      Integer initialPopulation,
      Integer denominator) {
    this(planDef, planDefUrl, name, expectedOutcome, initialPopulation, denominator, null, null);
  }

  public TestCaseInfo(
      String planDef,
      String planDefUrl,
      String name,
      ExpectedOutcome expectedOutcome,
      Integer initialPopulation,
      Integer denominator,
      Integer denomExclusion,
      Integer numerator) {
    this.planDef = planDef;
    this.planDefUrl = planDefUrl;
    this.name = name;
    this.initialPopulation = initialPopulation;
    this.denominator = denominator;
    this.denomExclusion = denomExclusion;
    this.numerator = numerator;
    this.expectedOutcome = expectedOutcome;
  }

  public String getPlanDef() {
    return this.planDef;
  }

  public String getPlanDefUrl() {
    return this.planDefUrl;
  }

  public String getName() {
    return this.name;
  }

  public ExpectedOutcome getExpectedOutcome() {
    return this.expectedOutcome;
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
