## Operator Precedence

The following are the precedence levels and associativity of the operators in the Pyr language. The higher the precedence, the faster the operation will be performed.

<table data-line="0" class="code-line" dir="auto">
<thead data-line="1" class="code-line" dir="auto">
<tr data-line="0" class="code-line" dir="auto">
<th data-line="0" class="code-line" dir="auto">Operator</th>
<th data-line="0" class="code-line" dir="auto">Description</th>
<th data-line="0" class="code-line" dir="auto">Associativity</th>
<th data-line="0" class="code-line" dir="auto">Precedence</th>
</tr>
</thead>
<tbody data-line="3" class="code-line" dir="auto">
<tr data-line="2" class="code-line" dir="auto">
<td data-line="2" class="code-line" dir="auto"><code>()</code>, <code>[]</code></td>
<td data-line="2" class="code-line" dir="auto">Function Call, Indexing</td>
<td data-line="2" class="code-line" dir="auto">Left-to-right</td>
<td data-line="2" class="code-line" dir="auto">11</td>
</tr>
<tr data-line="3" class="code-line" dir="auto">
<td data-line="3" class="code-line" dir="auto"><code>-</code>, <code>!</code></td>
<td data-line="3" class="code-line" dir="auto">Negation</td>
<td rowspan="2" data-line="3" class="code-line" dir="auto">Right-to-left</td>
<td data-line="3" class="code-line" dir="auto">10</td>
</tr>
<tr data-line="4" class="code-line" dir="auto">
<td data-line="4" class="code-line" dir="auto"><code>^</code></td>
<td data-line="4" class="code-line" dir="auto">Exponent</td>
<td data-line="4" class="code-line" dir="auto">9</td>
</tr>
<tr data-line="5" class="code-line" dir="auto">
<td data-line="5" class="code-line" dir="auto"><code>*</code>, <code>/</code>, <code>%</code></td>
<td data-line="5" class="code-line" dir="auto">Multiply, Division, Modulo</td>
<td rowspan="3" data-line="5" class="code-line" dir="auto">Left-to-right</td>
<td data-line="5" class="code-line" dir="auto">8</td>
</tr>
<tr data-line="6" class="code-line" dir="auto">
<td data-line="6" class="code-line" dir="auto"><code>+</code>, <code>-</code></td>
<td data-line="6" class="code-line" dir="auto">Addition, Subtraction</td>
<td data-line="6" class="code-line" dir="auto">7</td>
</tr>
<tr data-line="7" class="code-line" dir="auto">
<td data-line="7" class="code-line" dir="auto"><code>&lt;&lt;</code>, <code>&gt;&gt;</code></td>
<td data-line="7" class="code-line" dir="auto">Bitwise Shift Left, Bitwise Shift Right</td>
<td data-line="7" class="code-line" dir="auto">6</td>
</tr>
<tr data-line="8" class="code-line" dir="auto">
<td data-line="8" class="code-line" dir="auto"><code>&lt;</code>, <code>&lt;=</code>, <code>&gt;</code>, <code>&gt;=</code></td>
<td data-line="8" class="code-line" dir="auto">Less Than, Less Than or Equal, Greater Than, Greater Than or Equal</td>
<td rowspan="2" data-line="8" class="code-line" dir="auto">Not chainable</td>
<td data-line="8" class="code-line" dir="auto">5</td>
</tr>
<tr data-line="9" class="code-line" dir="auto">
<td data-line="9" class="code-line" dir="auto"><code>==</code>, <code>!=</code></td>
<td data-line="9" class="code-line" dir="auto">Equal, Not Equal</td>
<td data-line="9" class="code-line" dir="auto">4</td>
</tr>
<tr data-line="10" class="code-line" dir="auto">
<td data-line="10" class="code-line" dir="auto"><code>&amp;</code></td>
<td data-line="10" class="code-line" dir="auto">Bitwise AND</td>
<td rowspan="3" data-line="10" class="code-line" dir="auto">Left-to-right</td>
<td data-line="10" class="code-line" dir="auto">3</td>
</tr>
<tr data-line="11" class="code-line" dir="auto">
<td data-line="11" class="code-line" dir="auto"><code>|</code></td>
<td data-line="11" class="code-line" dir="auto">Bitwise OR</td>
<td data-line="11" class="code-line" dir="auto">2</td>
</tr>
<tr data-line="12" class="code-line" dir="auto">
<td data-line="12" class="code-line" dir="auto"><code>and</code>, <code>or</code></td>
<td data-line="12" class="code-line" dir="auto">Logical And, Logical Or</td>
<td data-line="12" class="code-line" dir="auto">1</td>
</tr>
<tr data-line="13" class="code-line" dir="auto">
<td data-line="13" class="code-line" dir="auto"><code>=</code></td>
<td data-line="13" class="code-line" dir="auto">Variable Assignment</td>
<td data-line="13" class="code-line" dir="auto">Right-to-left</td>
<td data-line="13" class="code-line" dir="auto">0</td>
</tr>
</tbody>
</table>

For more information about each of the operators, see [Arithmetic](./arithmetic.md), [Comparison](./comparison.md) and [Bitwise](./bitwise.md).
