describe("Selecting Features page accordion - mutual exclusion", () => {
  beforeEach(() => {
    cy.visit("/");
    cy.navigate_to_tab("Selecting Features");
    // Wait for the server to render the Time series card content
    cy.get('[data-testid="selectimeseries"]', { timeout: 10000 }).should('exist');
  });

  it("'Time series transformations' card starts open", () => {
    cy.get('[data-testid="selectimeseries"]', { timeout: 8000 }).should('be.visible');
  });

  it("'Training vectors options' card starts collapsed", () => {
    cy.get('[data-testid="temporalhorizon"]', { timeout: 8000 }).should('not.be.visible');
  });

  it("Opening 'Training vectors' collapses 'Time series transformations'", () => {
    cy.get('[data-testid="selectimeseries"]').should('be.visible');
    cy.toggle_card("toggle_tv_card");
    cy.get('[data-testid="temporalhorizon"]', { timeout: 8000 }).should('be.visible');
    cy.get('[data-testid="selectimeseries"]', { timeout: 8000 }).should('not.be.visible');
  });

  it("Opening 'Models options' collapses 'Training vectors'", () => {
    // First open training vectors
    cy.toggle_card("toggle_tv_card");
    cy.get('[data-testid="temporalhorizon"]').should('be.visible');
    // Now open models options
    cy.toggle_card("toggle_mo_card");
    // Training vectors should be hidden
    cy.get('[data-testid="temporalhorizon"]', { timeout: 8000 }).should('not.be.visible');
  });

  it("Opening 'Training options' collapses other open card", () => {
    cy.get('[data-testid="selectimeseries"]', { timeout: 8000 }).should('be.visible');
    cy.toggle_card("toggle_to_card");
    // Time series transformations card should collapse
    cy.get('[data-testid="selectimeseries"]', { timeout: 8000 }).should('not.be.visible');
  });

  it("Re-opening 'Time series transformations' collapses 'Training vectors'", () => {
    cy.toggle_card("toggle_tv_card");
    cy.get('[data-testid="temporalhorizon"]', { timeout: 8000 }).should('be.visible');
    cy.toggle_card("toggle_ts_card");
    cy.get('[data-testid="selectimeseries"]', { timeout: 8000 }).should('be.visible');
    cy.get('[data-testid="temporalhorizon"]', { timeout: 8000 }).should('not.be.visible');
  });
});
