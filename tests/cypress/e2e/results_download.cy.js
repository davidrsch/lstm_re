describe("Results download button disabled state", () => {
  beforeEach(() => {
    cy.visit("/");
    cy.navigate_to_tab("Results");
  });

  it("Download CommandBar button is disabled before an experiment completes", () => {
    cy.get('[role="menuitem"]')
      .filter((i, el) => Cypress.$(el).text().trim() === "Download")
      .should("have.attr", "aria-disabled", "true");
  });
});
