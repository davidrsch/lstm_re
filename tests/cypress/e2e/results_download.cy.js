describe("Results download button disabled state", () => {
  beforeEach(() => {
    cy.visit("/");
    cy.navigate_to_tab("Results");
  });

  it("Download CommandBar button is disabled before an experiment completes", () => {
    cy.contains('[role="menuitem"]', 'Download', { timeout: 8000 })
      .should('be.disabled');
  });
});
