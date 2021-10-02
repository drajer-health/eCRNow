package com.drajer.bsa.controller;

public class TestCaseInfo {

    private String planDef;
    private String name;
    private boolean shouldTrigger;
    private int initialPopulation;
    private int denominator;
    private int denomExclusion;
    private int numerator;
    public TestCaseInfo(String planDef, String name, boolean shouldTrigger, int initialPopulation, int denominator, int denomExclusion, int numerator)
    {
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

    public int getInitialPopulation() {
        return this.initialPopulation;
    }

    public int getDenominator() {
        return this.denominator;
    }

    public int getDenominatorExclusion() {
        return this.denomExclusion;
    }

    public int getNumerator() {
        return this.numerator;
    }
    
}
