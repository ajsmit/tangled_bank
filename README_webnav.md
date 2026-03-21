**Role and Objective**

You are an expert Quarto site architect and front-end designer. Your task is to redesign the navigation system of an existing Quarto-based academic website to improve usability, clarity, and structural coherence. The site contains multiple courses, deep hierarchical content, and long-form teaching material.

Your goal is to implement a modern, layered navigation system using Quarto-native features (YAML configuration, sidebar structures, navbar, TOC, and minimal CSS/JS where necessary). The redesign must remain fully compatible with Quarto.

---

**Context**

The current navigation has the following problems:

* The sidebar mixes global site navigation with course-specific navigation.
* Too many links are visible simultaneously, creating cognitive overload.
* Hierarchical structure is difficult to scan due to weak visual differentiation.
* Page-level navigation (table of contents) competes with course navigation.
* Navigation elements are duplicated (course titles, page titles, etc.).
* The design does not align with typical user tasks (progression, lookup, orientation).

---

**Design Principle**

When a user is inside a course page, show:

1. The course structure (only that course),
2. The current section within that course,
3. The internal structure of the current page.

All other site content should be accessible but not visible by default.

---

**Required Architecture**

Implement a three-layer navigation model:

1. **Global Navigation (Navbar)**

   * Contains only top-level site domains:

     * Home
     * Undergraduate
     * Honours Core
     * Electives
     * AI & Policy
     * Support
     * About
   * Must be defined in `website.navbar` in `_quarto.yml`
   * Must NOT appear in sidebars

2. **Course-Level Navigation (Sidebar)**

   * Each course (e.g., BCB744) must have its own dedicated sidebar
   * Sidebar must include:

     * Course title at top (linked to course overview page)
     * Logical grouping into sections (e.g., “Part I”, “Part II”, etc.)
     * Pages nested within sections
   * Only the current section should be expanded by default
   * All other sections should be collapsed
   * Sidebar must NOT include content from other courses

3. **Page-Level Navigation (TOC)**

   * Right-side table of contents for headings within the page
   * Sticky on scroll (desktop)
   * Visually lighter than sidebar
   * Must not duplicate course navigation

---

**Functional Requirements**

1. **Sidebar Behavior**

   * Collapsible sections (progressive disclosure)
   * Strong visual indicator of current page:

     * Background highlight
     * Bold text
     * Accent marker (e.g., left border or pill)
   * Clear typographic hierarchy between:

     * Section headers
     * Pages
     * Sub-pages

2. **Breadcrumbs**

   * Add breadcrumb navigation at top of each page:

     * Format: Home > Section > Course > Part > Page
   * Must reflect actual hierarchy

3. **Search**

   * Prominent search in navbar
   * Optional duplication at top of sidebar
   * Must support fast lookup of concepts and pages

4. **Sequential Navigation**

   * Add “Previous | Overview | Next” controls:

     * At top of page (compact)
     * At bottom (existing can remain)
   * Must follow course order

5. **Landing Pages**

   * Each course and major section should have a short index page:

     * Description
     * Structure overview
     * Key links
   * Sidebar should remain compact; landing pages carry explanatory load

---

**Visual Design Requirements**

* Increase vertical spacing between sidebar sections
* Reduce visual weight of lower-level items
* Use consistent sentence case
* Avoid long, cluttered labels where possible
* Ensure clear contrast between:

  * Active item
  * Inactive items
  * Section headers
* Avoid repeating titles unnecessarily

---

**Implementation Constraints**

* Use Quarto-native configuration wherever possible:

  * `_quarto.yml`
  * `sidebar:` definitions
  * `navbar:` definitions
  * `toc:` options
* Use minimal custom CSS for:

  * Sidebar highlighting
  * Spacing
  * Sticky TOC
* Use minimal JavaScript only if necessary for collapsible behavior beyond Quarto defaults
* Do not introduce external frameworks

---

**Deliverables**

1. Updated `_quarto.yml` including:

   * Clean navbar definition
   * Section-specific sidebars for each course
2. Sidebar structure for one example course (e.g., BCB744)
3. CSS snippet to:

   * Improve hierarchy and spacing
   * Highlight active page
   * Refine sidebar readability
4. Optional JS (only if needed) for improved collapsible behavior
5. Example of breadcrumb implementation
6. Brief explanation of how the new system improves:

   * Orientation
   * Scanning
   * Task flow

---

**Success Criteria**

* The sidebar shows only relevant course content
* Users can identify their location instantly
* Navigation supports both linear progression and targeted lookup
* Visual clutter is reduced without removing access to content
* The system is maintainable within Quarto

---

**Execution**

Analyse the current structure, then produce the full revised configuration and styling needed to implement this navigation system.
