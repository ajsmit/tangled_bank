# BCB733 Practical Exam Assessment Development (2025)

## ChatGPT and Claude for Assessment Development

### Step 1

***I write the following rough set of instructions:***

Please see the attached Word document. It has an exam paper given to the BCB744 class (Biostatistics) of 2025. 

In it, there are some broad Assessment Criteria that should roughly guide how a rubric is created (relative weightings of the expectations as demonstrated in the students' answers). It will not apply to each question, as some questions are more technical and objective than others. However, it should be able to be applied at the levels of the six tasks.

The rubric will guide an AI assessor about how to assign marks to the questions under Tasks 1 to 6. 

At the end of each task there is a mark in % (such as [10%] for Task 1) that weighs the contribution of that task's questions to the overall mark. The overall mark is 100%, and the proportional % allocations for each task add to 100%.

Task 1 has the following components: (1) and (2)

Task 2: 

* 2.1 (1) and (2)
* 2.2 (1), (2), and (3)
* 2.3

Task 3: (1), (2), (3), and (4)

Task 4:

* 4.1
* 4.2

Task 5:

* 5.1
* 5.2
* 5.3
* 5.4
* 5.5

Task 6

Please help me develop a rubric for the AI assessor to guide it in marking the exam assessments submitted by the students in a defensible, consistent (across students), and objective (as far as possible given the nature of each question) manner. The rubric can be developed specifically for each task, and the weightings overall can be guided by the Assessment Criteria. Please be guided by the "Note to assessor" that I provide with each answer in the uploaded PDF.

### Step 2

***Feed this rough set of instructions to your AI agent of choice. I used ChatGPT 4o.***

I get the following:

General Structure of the Rubric

Each Task is evaluated under the following axes:

1.	Technical Accuracy (50%)
2.	Depth of Analysis (20%)
3.	Clarity and Communication (20%)
4.	Critical Thinking (10%)

Each subcomponent is marked on a 0–100 scale, then scaled to its proportion of the task weight. For example, Task 5 is worth 30% of the total mark, so a sub-question like 5.1 (one of five) may contribute up to 6% if evenly weighted.

AI returns the following:

🔹 Task 1: Initial Processing [10%]

Weight within task:

- 1.1 Extraction and Restructuring (50%)
- 1.2 Conversion and Summarisation (50%)

Rubric:

- Technical Accuracy (50%)
- Correct unpacking of NetCDF variables (names, dimensionality): 15%
- Time conversion handled correctly (POSIX timestamps): 10%
- Data reshaped into appropriate long format: 15%
- Presence of appropriate columns (year, quarter, etc.): 10%
- Depth of Analysis (20%)
- Efficient use of methods (e.g. hyper_tibble() or expand.grid() vs brute loops): 10%
- Use of Cartesian indexing or equivalent vectorised operation: 10%
- Clarity and Communication (20%)
- Code is readable, well-commented: 10%
- Summary of the resulting data structure shown and interpretable: 10%
- Critical Thinking (10%)
- Indicates understanding of spatial × temporal structure and mentions NA implications: 10%

🔹 Task 2: Exploratory Data Analysis [10%]

2.1 Weighted Mean Time Series
- (1) Weighted mean across time: 15%
- (2) Time series for 100 pixels: 15%

2.2 Summary Statistics:

- (1) Descriptive stats: 20%
- (2) Visualisations: 20%
- (3) Interpretation: 20%

2.3 Observation Density Map: 10%

Rubric:

- Technical Accuracy (50%)
- Proper handling of weights and NA filtering: 10%
- Correct aggregation logic (quarter, pixel, etc.): 10%
- Appropriateness of visualisation syntax and ggplot conventions: 10%
- Use of statistical descriptors (mean, sd, skew, etc.) correctly: 10%
- Map projection/geodesic coordinates and section overlay accuracy: 10%
- Depth of Analysis (20%)
- Commentary on skewness, kurtosis, and statistical implications: 10%
- Recognition of seasonal/temporal signals in plots and stats: 10%
- Clarity and Communication (20%)
- Plot labels, axes, titles intelligible and precise: 10%
- Logical narrative supporting visualisations/statistics: 10%
- Critical Thinking (10%)
- Justification of metric choices, handling of anomalous years: 5%
- Suggestions of ecological explanations (e.g., photoperiod, storminess): 5%

🔹 Task 3: Inferential Statistics I [20%]

Weight within task:

- (1) Hypotheses: 10%
- (2) Model selection and justification: 20%
- (3) Assumption testing: 20%
- (4) Result interpretation and diagnostics: 50%

Rubric:

- Technical Accuracy (50%)
- Correct use of linear model and specification (additive, no interaction): 20%
- Explicit assumptions tested (normality, homogeneity): 10%
- Proper model diagnostics and visual checks: 10%
- Use of correct significance thresholds and p-value interpretation: 10%
- Depth of Analysis (20%)
- Justification for using aggregate means vs raw data: 10%
- Consideration of alternative models (e.g., GAMs): 10%
- Clarity and Communication (20%)
- Hypotheses stated cleanly, concisely: 10%
- Figure/Table references integrated smoothly in the narrative: 10%
- Critical Thinking (10%)
- Recognition of model limitations and implications (e.g. low R²): 10%

🔹 Task 4: Spatial Assignment [10%]

4.1 Section Assignment: 5%

4.2 Bioregion Assignment: 5%

Rubric:

- Technical Accuracy (50%)
- Correct application of Haversine formula or great-circle logic: 20%
- Accurate section_id assignment: 10%
- Bioregion mapping via join or merge: 10%
- Correct data columns preserved/renamed: 10%
- Depth of Analysis (20%)
- Efficiency of matching routine (e.g., mapply() or vectorised join): 10%
- Consideration of spatial boundaries (e.g., limiting to section 1–22): 10%
- Clarity and Communication (20%)
- Annotated code, explanation of proximity logic: 10%
- Output (head(), summary(), tail()) shows assignment integrity: 10%
- Critical Thinking (10%)
- Considers effect of section resolution or mapping error: 10%

⸻

🔹 Task 5: Inferential Statistics II [30%]

Each sub-task contributes approximately 6% unless reweighted explicitly.

Rubric per sub-task (5.1–5.5):

- Technical Accuracy (50%)
- Model type (ANOVA, LM, ANCOVA) appropriate: 15%
- Correct test execution (summary, diagnostics): 15%
- Assumptions evaluated, violations addressed: 10%
- Non-parametric alternative proposed when appropriate: 10%
- Depth of Analysis (20%)
- Explicit rationale for model choice: 10%
- Discussion of structure in data (nesting, lack of interaction): 10%
- Clarity and Communication (20%)
- Hypotheses clearly and formally stated: 10%
- Visualisations appropriately labelled and explained: 10%
- Critical Thinking (10%)
- Insight into ecological implications of findings (e.g., BMP trend): 10%

Add 1–2 bonus marks if:

- Multicollinearity (e.g., VIF) or autocorrelation (e.g., DW test) is discussed
- Advanced diagnostics (e.g., Breusch–Pagan, TukeyHSD) are used correctly

⸻

🔹 Task 6: Write-up [10%]

Rubric:

- Technical Accuracy (50%)
- Consistent reference to previous results, correct figure/table interpretation: 25%
- Accurate paraphrasing of statistical results: 15%
- Adherence to 2-page length limit, integration of material: 10%
- Depth of Analysis (20%)
- Rich synthesis across Tasks 2–5, not isolated repetition: 10%
- Conceptual connection of seasonality, trend, and spatial heterogeneity: 10%
- Clarity and Communication (20%)
- Coherent scientific writing style, flowing paragraph structure: 10%
- Effective integration of figure references and literature: 10%
- Critical Thinking (10%)
- Limitations clearly acknowledged and reflected on: 5%
- Forward-looking ecological insight or recommendation offered: 5%

### Step 3 

Edit if necessary and save as a PDF ("BCB744_Prac_Exam_Rubric_2025.pdf") -- used below.

### Step 4

Develop assessment guideline. Specify how marks should be returned and granularity of mark allocation and feedback:

***Use AI to develop the following assessment instructions based on:***

Prompt: Please expand the instructions below into an unambiguous set of instructions that an AI can follow to correct apply the rubric. See attached rubric.

Apply the rubric "BCB744_Prac_Exam_Rubric_2025.pdf" to each student's exam paper. Apply consistent, repeatable, defensible, and clear assessment according to this rubric. I want the following levels and details of reporting:

A. A .txt file for each student, named according to the filename of the student's answer paper, with:

- An overall narrative feedback for each Task according the rubric guidelines; what are the good and less good points the student should consider? These should be useful to guide the leaning process in the future.
- For each question within a task (finest level of granularity), the mark allocated (e.g. 35/50 for Task 1 (1.1) or 10/15 for Task 2.1 (1))
- The overall mark for the task, e.g. for "Task 1: Initial Processing [10%]" the mark would be for example 8/10 (these marks should add up across all tasks to a value of out 100, the final mark in %)

B. A .csv file, named according to the filename of the student's answer paper, with:

- The mark for each task
- The total mark in % (sum of the marks for a task, above)

AI returns the following:

Assessment Instructions for BCB744 Practical Exam (2025)

INPUT

- Each student’s response will be in a .docx or .pdf output file.
- The worked 
- The rubric is defined in BCB744_Prac_Exam_Rubric_2025.pdf (attached).
- The model answers I developed for the assessment as BCB744_Biostats_Prac_Exam_2025.docx.
- Assessment criteria apply per task and question, with overall weightings per task provided.


STEP-BY-STEP ASSESSMENT INSTRUCTIONS AND PROCEDURE

STEP 1: Parse and identify the student file

1.	Read the student answer file.
2.	Identify and extract answers corresponding to:
  - Task 1 (with subcomponents 1.1 and 1.2)
  - Task 2.1 (1 and 2), 2.2 (1, 2, 3), and 2.3
  - Task 3 (1–4)
  - Task 4.1 and 4.2
  - Task 5.1 through 5.5
  - Task 6 (Write-up)

STEP 2: Evaluate each component using the rubric

For each sub-question or component:

1.	Apply the rubric section relevant to that task:
  - Use the four assessment dimensions:
  - Technical Accuracy (50%)
  - Depth of Analysis (20%)
  - Clarity and Communication (20%)
  - Critical Thinking (10%)
  - Each is scored on a 0–100 scale for that component.
2.	Multiply each score by the weighting for that component as defined in the rubric:
  - E.g., Task 1.1 is 50% of Task 1 (worth 10%), so max contribution is 5 points.
  - Task 5.3 is one of five sub-tasks in Task 5 (30% total), so it’s ~6%.
3.	Tally sub-task scores to compute the task total (e.g., Task 3 might yield 17.4/20).
4.	Round task scores to one decimal place.

STEP 3: Write feedback and save to .txt

For each student, generate a .txt file named identically to their input file (but with .txt extension):

A. Feedback Report Structure

1.	Narrative feedback for each task (Tasks 1–6)
  - One paragraph per task.
  - Highlight:
  - Strengths (e.g., well-structured code, clear visualisations)
  - Weaknesses (e.g., incorrect model use, insufficient explanation)
  - Areas for improvement (e.g., mention VIF or DW test next time)
  - Must be constructive and written for student learning.
2.	Marks per component
  - Use format: Task 1.1: 43/50 or Task 2.2 (2): 12/20
  - One line per sub-question (lowest possible granularity)
3.	Task total
  - Use format: Task 1: 8.6/10
4.	Final total
  - Use format: Total mark: 84.5/100

STEP 4: Generate .csv with marks

For the same student, create a .csv file (named identically but with .csv extension) with the following structure:

Task	Mark
Task 1	8.6
Task 2	9.2
Task 3	17.4
Task 4	9.0
Task 5	27.0
Task 6	9.3
Total	80.5

ADDITIONAL GUIDELINE FOR CONSISTENCY

- Use the same rubric for all students.
- Apply point deductions proportionally across the four dimensions of the rubric.
- Do not penalise for choices beyond the scope of the taught material (e.g., not using mixed models).
- Award partial marks for attempts that demonstrate correct reasoning, even if syntax is flawed.
- Always refer to the original “Notes to Assessor” where included for guidance on expected answers.

SUMMARY

Output Type	Content:

- .txt	Narrative feedback, component marks, task marks, total mark
- .csv	Tabular summary of marks per task + total

### Prompt

**Prompt ChatGPT or Clause Sonnet 4:** Please refer to the Project knowledge for detailed instructions. Produce the required .txt and .csv files as specified according to the instructions. The first batch of five student submissions is attached here.

## Use Gemini for Assessment Schema Development

### -------------------------------------------------------------------------- ##
## Trying new marking guidelines for Gemini
### -------------------------------------------------------------------------- ##

Please see the attached file, "BCB744_Biostats_Prac_Exam_2025.docx". It contains an exam given to the BCB744 Biostatistics students. There are six tasks, Tasks 1-6. For each question within the tasks, I have provided the ideal (model) answer according to which the student's work must be assessed. Below is outlined a set of instructions to give to an AI system to apply a consistent, defensible, reproducible, justifiable, and verifiable mark for each task.

### GENERAL ASSESSMENT PROCEDURE AND STANDARDS (Apply to All Tasks)

#### A. EVALUATION AXES (Apply to All Tasks)

1. Technical Accuracy (50%)
2. Depth of Analysis (20%)
3. Clarity and Communication (20%)
4. Critical Thinking (10%)

#### B. ASSESSOR NOTES

Consider also the "Note(s) to Assessor" embedded in "BCB744_Biostats_Prac_Exam_2025.docx" for each task and the answer paper in general.

#### B. CRITICAL FORMATTING AND PRESENTATION REQUIREMENTS (Apply to All Tasks)

1. Automatic Penalties (Applied Before Task-Specific Assessment):
  - Code Execution Failure: Any question where code produces error messages and fails to generate required output = 0 marks for that specific question
  - Document Structure and Formatting [-15%]:
    - Poor document organisation without logical heading hierarchies
    - Untidy formatting that fails to resemble professional scientific presentation
    - Inconsistent or missing section numbering
    - Poor use of Quarto/markdown formatting features
  - Inappropriate Content Placement [-10% per occurrence]:
    - Long-form text answers written within code blocks instead of markdown text.
    - Explanatory text that should be in full sentences placed as code comments.
    - Results interpretation embedded in code rather than proper text sections.
  - Excessive Unnecessary Output [-15%]:
    - Long, unnecessary data printouts that serve no analytical purpose.
    - Acceptable outputs: `head()`, `tail()`, `glimpse()`, `summary()` when specifically required.
    - Penalised outputs: Full dataset prints, verbose model outputs without purpose, repetitive diagnostic information.
  - Poor Written Communication [-10% per occurrence]:
    - Text answers written as bullet points lacking explanatory depth.
    - Fragmented responses without complete sentences.
    - Lack of professional scientific writing style.
    - Missing transitions between analytical steps.
2. Presentation Standards Expected:
  - Document Structure: Clear hierarchical headings (Task > Subtask > Components).
  - Code Quality: Clean, commented, executable code with logical organisation.
  - Text Quality: Full sentences, professional tone, clear explanations between code blocks.
  - Output Management: Only essential outputs displayed, properly formatted tables/figures.
  - Scientific Style: Results presented as they would appear in a peer-reviewed publication.
3. Assessment Priority:
  - First: Check for automatic penalty conditions.
  - Second: Assess technical accuracy within each task.
  - Third: Evaluate depth, communication, and critical thinking.
  - Final: Apply task weightings to calculate overall mark.

#### C. GENERAL MARKING GUIDELINES (Apply to All Tasks)

1. Partial Credit:
  - Award partial credit for incomplete but methodologically sound approaches.
  - Recognise correct identification of appropriate methods even if not fully implemented.
  - Give credit for proper assumption testing even when assumptions are violated.
  - Award marks for reasonable alternative approaches that demonstrate understanding.
2. Bonus Considerations:
  - Additional marks for sophisticated analyses beyond requirements.
  - Credit for creative visualisations that enhance understanding.
  - Recognition of advanced statistical considerations (e.g., multiple comparisons, effect sizes).
  - Bonus for proper handling of complex design issues.
3. Common Deductions:
  - Poor code organisation and lack of comments.
  - Missing assumption testing.
  - Inappropriate figure quality or labelling.
  - Failure to address specific question requirements.
  - Plagiarism or lack of original analysis.

### FINAL MARK CALCULATION SCHEMA

- Task 1: Initial Processing
  - [Task Weight: 10%]
  - [Components (1) and (2) marked on a 0–100 scale, then scaled to equal proportions of the Task Weight of 10%]
- Task 2: Exploratory Data Analysis
  - [Task Weight: 10%]
  - [Tasks 2.1, 2.2, and 2.3, each marked on a 0–100 scale, then scaled to equal proportions of the Task Weight of 10%]
- Task 3: Inferential Statistics (Part 1)
  - [Task Weight: 20%]
  - [Components (1), (2), (3), and (4) each marked on a 0–100 scale, then scaled to equal proportions of the Task Weight of 20%]
- Task 3: Inferential Statistics (Part 1)
  - [Task Weight: 20%]
  - [Components (1), (2), (3), and (4) each marked on a 0–100 scale, then scaled to equal proportions of the Task Weight of 20%]
- Task 5: Inferential Statistics (Part 2)
  - [Task Weight: 30%]
  - [Tasks 5.1, 5.2, 5.3, 5.4, and 5.5 each marked on a 0–100 scale, then scaled to equal proportions of the Task Weight of 30%]
- Task 6: Write-up
  - [Task Weight: 10%]

- Total mark = (Task 1 × 0.10) + (Task 2 × 0.10) + (Task 3 × 0.20) + (Task 4 × 0.20) + (Task 5 × 0.30) + (Task 6 × 0.10)

### FEEDBACK INSTRUCTIONS

1. A .txt file for each student, named according to the filename of the student's answer paper, with:
  - An overall narrative feedback for each Task according to the "GENERAL ASSESSMENT PROCEDURE AND STANDARDS" (above); what are the good and less good points the student should consider? These should be useful to guide the learning process in the future. Refer to individual question subcomponents with specific examples where their answers were deemed insufficient, and offer actionable advice for improvement.
  - A listing of where and why deductions were applied.
  - The overall mark for the task, e.g. for "Task 1: Initial Processing [10%]" the mark would be, for example, 8/10.
2. A .csv file, named according to the filename of the student's answer paper, with:
  - The mark for each task
  - The total mark in % (sum of the marks for a task, above)

### ASSESSMENT INSTRUCTIONS

1. Read the student answer file.
2. Identify answers corresponding to:
  - Task 1 (with components 1.1 and 1.2)
  - Task 2.1 (components 1 and 2), 2.2 (1, 2, 3), and 2.3
  - Task 3 (components 1, 2, 3, and 4)
  - Task 4.1 and 4.2
  - Task 5.1 through 5.5
  - Task 6
3. For each student submission, assess their answers relative to the model answer and rubric in "BCB744_Biostats_Prac_Exam_2025.docx". Apply the marking consistently according to "GENERAL ASSESSMENT PROCEDURE AND STANDARDS", above.
4. Apply the "FINAL MARK CALCULATION SCHEMA", above.
5. Provide feedback as per "FEEDBACK INSTRUCTIONS", above.

## Refinement by Gemini

**Prompt 1:** Will this serve as clear instructions for an AI like yourself on assessing student exam papers?

Study feedback...

**Prompt 2:** Please provide me with an improved version of my instructions that considers all your recommendations. I have also attached the "BCB744_Biostats_Prac_Exam_2025.docx" file for your reference.

## Use Refined Feedback 

**Prompt Gemini:** Paste this prompt into ChatGPT together with "BCB744_Biostats_Prac_Exam_2025.docx":

**Preamble**

These instructions are for an AI system to assess student exam papers for the BCB744 Biostatistics module. The assessment must be consistent, defensible, reproducible, justifiable, and verifiable. The AI must have the capability to:

* Read and interpret student submissions (Quarto-generated `.docx` or `.pdf` files).
* Read, interpret, and apply the rules within this instruction set.
* Process and understand the entirety of the "BCB744_Biostats_Prac_Exam_2025.docx" (Exam Document), including all task descriptions, model answers/code, embedded "Note(s) to Assessor", and stated assessment criteria, as these form the primary basis for evaluation.
* Verify R code snippets and output relative the the model answer.
* Generate structured feedback and marks according to the schemas provided.

**I. GENERAL ASSESSMENT PROCEDURE AND STANDARDS (Apply to All Tasks unless overridden by task-specific notes in the Exam Document)**

**A. EVALUATION AXES (Applied to each assessable component within each Task)**

For each question/sub-component within a Task, a raw score (e.g., on a 0–100 scale, before task-specific scaling and weighting) will be determined based on the following axes. The primary reference for judging performance against these axes is the corresponding model answer, code, and "Note(s) to Assessor" in the Exam Document.

1.  **Technical Accuracy (50% of component mark)**
    * Correct application of data analyses and statistical methods as per the Exam Document's model answers[cite: 3].
    * Statistical tests must address the hypotheses appropriately, aligning with methods taught in BCB744[cite: 4].
    * Appropriate use of R packages, functions, and syntax, including code style and liberal commenting[cite: 8].
    * Correct choice and justification of techniques, including due consideration for the assumptions of methods used[cite: 8].
    * Accurate calculations and interpretation of results, including appropriate precision (e.g., decimal places)[cite: 8].
    * **Leniency Note:** Assessment will consider that students may not possess advanced statistical knowledge. For instance, if simpler taught methods (e.g., ANOVA, simple linear models) are used where more complex ones (e.g., LMMs) might be theoretically optimal but were not taught, the simpler approach, if correctly applied, is acceptable[cite: 5, 6].
    * **Non-Parametric Tests:** If non-parametric alternatives are required (e.g., due to assumption violations), marks are awarded for correctly identifying the appropriate test, even if the test itself is not executed (unless execution is specifically requested and part of the model answer)[cite: 7].
    * **Task 5 Specific:** For Task 5 questions, if students apply advanced models like LMEs without demonstrating a clear understanding of why they are necessary or appropriate for the specific question (given the scope of BCB744), the mark for that specific answer component (e.g., 5.1, 5.2) should be 0% for Technical Accuracy related to model choice and implementation[cite: 201].

2.  **Depth of Analysis (20% of component mark)**
    * Comprehensive exploration of the problem as demonstrated in the student's approach and interpretation[cite: 3].
    * Insightful interpretation of results, going beyond superficial statements[cite: 3].
    * Consideration of analytical shortfalls (due to data limitations, assumptions, etc.) and suggestions for improvement, where appropriate and guided by the Exam Document[cite: 3].
    * Application of "out-of-the-box" thinking if relevant and supported by the analysis[cite: 3].
    * The "Note(s) to Assessor" and model answers in the Exam Document for each question provide the specific benchmark for expected depth.

3.  **Clarity and Communication (20% of component mark)**
    * Logical organization of ideas, including clear section headings and subheadings (as outlined in "Critical Formatting and Presentation Requirements" below)[cite: 3, 19].
    * Clear and concise explanations at each stage of the analysis[cite: 3].
    * Effective use of publication-quality visualizations where appropriate, including all necessary annotations (labels, titles, legends)[cite: 3, 9].
    * Communication of results in a style appropriate for a scientific audience (e.g., resembling a journal article)[cite: 3].
    * Refer to model answers and visualisations in the Exam Document as the standard.

4.  **Critical Thinking (shown in conclusions/interpretations) (10% of component mark)**
    * Discussion of findings in the context of the problem (e.g., adding ecological context as appropriate)[cite: 3].
    * Identification of limitations of the analysis or data[cite: 3].
    * Discussion of assumptions of the methods used[cite: 3].
    * Consideration of broader implications of the findings, where applicable[cite: 3].
    * This is primarily assessed in interpretive sections and Task 6, but elements may apply to interpretations within other tasks.

**B. "NOTE(S) TO ASSESSOR" FROM EXAM DOCUMENT**

All "Note(s) to Assessor" embedded within the Exam Document for general assessment or specific tasks/questions are **primary directives**. They provide specific criteria, define expected outputs, highlight areas of leniency or strictness, and may supersede general guidelines in this document if a conflict arises for a particular question. The AI must treat these notes as key rules for assessment.

**C. CRITICAL FORMATTING, PRESENTATION, AND SPECIFIC TASK PENALTIES**

Penalties are applied sequentially as listed.

1.  **Automatic Zero Mark (Question-Specific):**
    * **Code Execution Failure:** Any question or sub-component requiring code-generated output where the student's code produces error messages and fails to generate the required output receives 0 marks for that *specific question/sub-component*[cite: 10].

2.  **Task-Specific Penalties (Applied to the raw mark of the specific task/question before weighting, as detailed in the Exam Document):**
    * **Task 1 - Requesting Processed Data:** If the student requests and uses the pre-processed CSV file instead of reading the NetCDF file, a 10% penalty is applied to the final mark calculated for Task 1[cite: 37]. (e.g., if Task 1 raw score is 80/100, it becomes 72/100).
    * **Task 6 - AI-Generated Text:** If text in the Task 6 write-up is identified as clearly AI-generated, subtract 50% from the mark awarded to Task 6[cite: 319]. (e.g., if Task 6 raw score is 70/100, it becomes 35/100).

3.  **Question/Sub-Component Specific Penalties (Applied to the raw mark of the specific question/sub-component where the infraction occurs, before weighting):**
    * **Inappropriate Content Placement:** -10% from that specific question/sub-component's mark for *each occurrence* where[cite: 11]:
        * Long-form text answers are written within code blocks.
        * Explanatory text that should be in complete sentences is placed only as code comments.
        * Results interpretation is embedded in code rather than in proper text sections.
    * **Poor Written Communication (Format):** -10% from that specific question/sub-component's mark where text answers are written primarily as bullet points and lack detailed explanatory power or full sentences where expected[cite: 12].

4.  **Global Penalties (Applied as a percentage deduction from the student's *final calculated overall exam mark* at the very end. These penalties can stack, but the total global penalty deduction cannot exceed 40% of the total exam mark. The final exam mark cannot be reduced below 0%.)**
    * **Document Structure and General Formatting Issues:** [-15% from final overall exam mark] if the document exhibits[cite: 13]:
        * Poor overall document organization without logical heading hierarchies (Task > Subtask > Components, as per Exam Document structure [cite: 19]).
        * Generally untidy formatting that fails to resemble a professional scientific presentation or the model answers in the Exam Document.
        * Inconsistent or missing section numbering.
        * Poor use of Quarto/markdown formatting features.
    * **Excessive Unnecessary Output:** [-15% from final overall exam mark] if the document contains[cite: 13]:
        * Long, unnecessary data printouts that serve no analytical purpose.
        * *Acceptable outputs*: `head()`, `tail()`, `glimpse()`, `summary()` when specifically required or appropriate for a brief overview.
        * *Penalized outputs*: Full dataset prints, verbose model outputs without purpose, repetitive diagnostic information not directly contributing to the interpretation or requested by the task.

**D. PRESENTATION STANDARDS EXPECTED (Inform qualitative assessment for "Clarity and Communication" and "Document Structure" penalty)**

* **Document Structure:** Clear hierarchical headings (e.g., Task 1, Task 1.1, Task 1.1.1) that mirror the Exam Document structure[cite: 19].
* **Code Quality:** Clean, well-commented, and executable R code with logical organization. Liberal commenting is encouraged[cite: 8].
* **Text Quality:** Full sentences, professional tone, clear explanations between code blocks. Avoid jargon where simpler language suffices, but maintain scientific rigor.
* **Output Management:** Only essential outputs displayed. Tables and figures should be properly formatted and referenced.
* **Scientific Style:** Results presented as they would appear in a peer-reviewed scientific publication[cite: 3, 9]. This includes appropriate figure/table captions and references in the text if multiple figures/tables are presented.

**E. ASSESSMENT WORKFLOW AND PRIORITY**

1.  **Initial Pass - Identify Answers:** For each student submission, parse the `.docx` or `.pdf` document to identify answers corresponding to each Task and its sub-components as outlined in the "ASSESSMENT INSTRUCTIONS" (Section III).
2.  **Component-Level Assessment:** For each sub-component of each Task:
    a.  Evaluate against the **Evaluation Axes (I.A)**, using the Exam Document's model answers, code, and "Note(s) to Assessor" as the primary reference. Assign a raw score (e.g., 0-100).
    b.  Apply any relevant **Question/Sub-Component Specific Penalties (I.C.3)** to this raw score.
3.  **Task-Level Score Calculation:**
    a.  Scale the raw scores of sub-components to their specified proportions within the Task (as per "FINAL MARK CALCULATION SCHEMA," Section II).
    b.  Sum scaled component scores to get the raw Task score.
    c.  Apply any **Task-Specific Penalties (I.C.2)** to this raw Task score.
4.  **Overall Exam Mark Calculation:**
    a.  Apply Task weightings to each Task score and sum them to calculate the provisional overall exam mark (as per "FINAL MARK CALCULATION SCHEMA," Section II).
5.  **Global Penalty Application:**
    a.  Apply any **Global Penalties (I.C.4)** to the provisional overall exam mark to arrive at the final overall exam mark.
6.  **Feedback Generation:** Generate feedback as per "FEEDBACK INSTRUCTIONS" (Section IV).

**F. GENERAL MARKING GUIDELINES**

1.  **Partial Credit:**
    * Award for incomplete but methodologically sound approaches.
    * Recognize correct identification of appropriate methods even if not fully implemented[cite: 7].
    * Give credit for proper assumption testing even when assumptions are violated (the discussion of violations is important).
    * Award marks for reasonable alternative approaches that demonstrate understanding, provided they are justified and align with the learning objectives of BCB744[cite: 5, 6].
2.  **Bonus Considerations:**
    * Specific Bonus Points:
        * **Task 5.1 - TukeyHSD:** If a TukeyHSD test is correctly performed and interpreted (though not explicitly required), add 2 marks to the raw score of Task 5.1 (component score capped at 100)[cite: 214, 215].
    * General Bonus Points (AI should look for these and flag for potential human review or apply if confident):
        * Award up to +5% to a specific *Task's raw score* (before weighting, task score capped at its maximum, e.g., 100) for exceptional work within that task, such as:
            * Sophisticated analyses or interpretations that are correct, relevant, and demonstrably beyond the direct requirements but still within the conceptual framework of the module.
            * Particularly insightful or creative visualisations that significantly enhance understanding beyond the model answer's standard.
            * Recognition and correct discussion of advanced statistical considerations (e.g., effect sizes if not explicitly asked for, nuanced handling of multiple comparisons) relevant to the question.
            * Proper handling and discussion of complex data/design issues beyond basic expectations.
3.  **Common Deductions (Checklist - many are covered by penalties or evaluation axes):**
    * Poor code organisation and lack of comments (addresses "Clarity" and "Technical Accuracy").
    * Missing or inadequate assumption testing for statistical models (addresses "Technical Accuracy" and "Depth").
    * Inappropriate figure quality or missing labels/legends (addresses "Clarity").
    * Failure to address specific requirements of a question component (addresses "Technical Accuracy").
    * Evidence of plagiarism or lack of original analysis (AI should flag this for human review; Task 6 has a specific penalty for AI-generated text [cite: 319]).

**II. FINAL MARK CALCULATION SCHEMA**

1.  **Task Component Scaling:**
    * **Task 1: Initial Processing** [Task Weight: 10%]
        * Components (1) and (2) are each marked on a 0–100 scale. These two component scores are then averaged to get the Task 1 raw score.
    * **Task 2: Exploratory Data Analysis** [Task Weight: 10%]
        * Tasks 2.1, 2.2, and 2.3 are each marked on a 0–100 scale. These three component scores are then averaged to get the Task 2 raw score.
    * **Task 3: Inferential Statistics (Part 1)** [Task Weight: 20%]
        * Components (1), (2), (3), and (4) are each marked on a 0–100 scale. These four component scores are then averaged to get the Task 3 raw score.
    * **Task 4: Assigning Kelp Observations** [Task Weight: 20%]
        * Task 4.1 is marked on a 0-100 scale. Task 4.2 is marked on a 0-100 scale.
        * Task 4 raw score = (Task 4.1 score × 0.70) + (Task 4.2 score × 0.30).
    * **Task 5: Inferential Statistics (Part 2)** [Task Weight: 30%]
        * Tasks 5.1, 5.2, 5.3, 5.4, and 5.5 are each marked on a 0–100 scale. These five component scores are then averaged to get the Task 5 raw score.
    * **Task 6: Write-up** [Task Weight: 10%]
        * Marked on a 0–100 scale. This is the Task 6 raw score.

2.  **Apply Task-Specific Penalties (I.C.2) to relevant raw Task scores.**

3.  **Calculate Provisional Overall Exam Mark:**
    * Provisional Mark = (Task 1 final raw score × 0.10) + (Task 2 final raw score × 0.10) + (Task 3 final raw score × 0.20) + (Task 4 final raw score × 0.20) + (Task 5 final raw score × 0.30) + (Task 6 final raw score × 0.10).

4.  **Apply Global Penalties (I.C.4) to the Provisional Overall Exam Mark to get the Final Overall Exam Mark.**

**III. ASSESSMENT INSTRUCTIONS (AI Operational Steps)**

1.  **Ingest Data:** Load the student's submitted `.docx` or `.pdf` answer file and the "BCB744_Biostats_Prac_Exam_2025.docx" (Exam Document).
2.  **Parse Submission:** Identify student's answers corresponding to:
    * Task 1 (Components 1.1, 1.2) [cite: 28]
    * Task 2 (Components 2.1 including sub-parts, 2.2 including sub-parts, 2.3)
    * Task 3 (Components 1, 2, 3, 4) [cite: 112]
    * Task 4 (Components 4.1, 4.2) [cite: 162]
    * Task 5 (Components 5.1, 5.2, 5.3, 5.4, 5.5) [cite: 194]
    * Task 6 (Write-up) [cite: 313]
3.  **Evaluate and Score:** For each student submission, apply the "ASSESSMENT WORKFLOW AND PRIORITY" (I.E) rigorously.
    * Assess answers relative to the model answers, code, rubrics, and "Note(s) to Assessor" in the Exam Document.
    * Apply penalties and bonuses as defined.
    * Calculate marks consistently according to the "FINAL MARK CALCULATION SCHEMA" (II).
4.  **Generate Feedback:** Provide feedback as per "FEEDBACK INSTRUCTIONS" (IV).

**IV. FEEDBACK INSTRUCTIONS**

For each student submission, generate two files:

1.  **A `.txt` file**, named `[StudentFilename]_feedback.txt`:
    * **Overall Narrative Feedback per Task:** For each Task (1-6), provide a summary.
        * Refer to the **Evaluation Axes (I.A)**. What were the good points and areas for improvement for that Task?
        * Provide specific examples from the student's paper where answers were strong or insufficient, referencing sub-components (e.g., "In Task 2.1, the plot was missing axis labels which affects Clarity.").
        * Offer actionable advice for future improvement.
        * Reference relevant "Note(s) to Assessor" from the Exam Document if they significantly influenced the marking of that task.
    * **List of Deductions:** Clearly list all penalties applied (Question-specific, Task-specific, Global), explaining where and why each was applied.
    * **Mark per Task:** State the final mark for each task after all relevant deductions and scaling, showing it as a fraction of its weight (e.g., "Task 1: Initial Processing [10%]: Your mark 8/10").
    * **Final Overall Mark:** State the final overall percentage.

2.  **A `.csv` file**, named `[StudentFilename]_marks.csv`, with the following columns:
    * `StudentID` (if available, otherwise `StudentFilename`)
    * `Task1_Score` (out of 10)
    * `Task2_Score` (out of 10)
    * `Task3_Score` (out of 20)
    * `Task4_Score` (out of 20)
    * `Task5_Score` (out of 30)
    * `Task6_Score` (out of 10)
    * `Provisional_Total_Percent` (sum of task scores, before global penalties)
    * `Global_Penalties_Applied_Percent` (total % deducted globally)
    * `Final_Total_Percent` (final mark after all deductions)

