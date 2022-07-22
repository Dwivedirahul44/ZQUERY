# ZQUERY
Open SQL executing/testing application on SAP AS ABAP

<details><summary><h2>Installation</h2></summary>
 Upload files and screens to your system and add GUI status manually by following below provided instructions( SAP doesnt directly allow download/upload interface for GUI-statuses,you will abapgit or SAPLINK to download and upload them ).
## Adding GUI-statuses
Right click on program name and choose create -> Gui Status
![image](https://user-images.githubusercontent.com/43263062/173181509-a89a329a-91c3-472f-b6c8-1e03d5a8970c.png)
<details><summary><h3>Adding STATUS (Example)</h3></summary>
Create status
![image](https://user-images.githubusercontent.com/43263062/173181999-c036f6f3-d361-475e-bb1a-0a7428fb585d.png)

You will be presented with the status screen,
![image](https://user-images.githubusercontent.com/43263062/173181668-92a03c06-f48c-4f33-a95c-ef6e37663992.png)

add item to application toolbar,
Add Fuction code to first Input: NEW
![image](https://user-images.githubusercontent.com/43263062/173182403-c89afe22-3e41-4439-987a-1f1caa64978f.png)
select OK.
![image](https://user-images.githubusercontent.com/43263062/173181728-ad9d9119-d52e-44e7-af1e-c5a6121ef233.png)
Double click and select F5 or any function key of your choice and select OK.
![image](https://user-images.githubusercontent.com/43263062/173181737-e0648bca-c709-4b47-a4f7-db430090e31e.png)

fill relavant Info and save.
![image](https://user-images.githubusercontent.com/43263062/173181811-7a980291-cd9e-4d5f-bc55-8c7fa3105c97.png)
</details>
 
Below table contains list of Statuses/Items to be added:
 <table>
  <thead>
   <tr><th>Status Name</th><th>Short Text</th><th>Status Type</tr>
  </thead>
  <tbody>
   <tr><th>PF_9000</th><th>Main Status</th><th>Normal Screen</tr>
   <tr><th>PF_9100</th><th>Query Wizard</th><th>Dialog Box</tr>   
   <tr><th>PF_9200</th><th>Setting</th><th>Dialog Box</tr>  
   <tr><th>STATUS9300</th><th>Save Option</th><th>Dialog Box</tr>    
  </tbody>
 </table>
<details><summary><h3>PF_9000</h3></summary>
Application toolbar items
   <table>
  <thead>
   <tr><th>Function Code</th><th>Type</th><th>Function Text</th><th>Icon Name</th><th>Icon Text</th><th>Info.Text</th><th>Field Name</th></tr>
  </thead>
  <tbody>
   <tr><th>NEW</th><th>Static Text</th><th>NEW</th><th>ICON_ADD_ROW</th><th></th><th>New Tab</th><th></th></tr>  
   <tr><th>DEL_TAB</th><th>Static Text</th><th>DEL TAB</th><th>ICON_REMOVE_ROW</th><th></th><th>Delete Tab</th><th></th></tr>
   <tr><th>separator Line</th></tr>
   <tr><th>QWIZ</th><th>Static Text</th><th>Query Wizard</th><th>ICON_WIZARD</th><th></th><th>Query Wizard</th><th></th></tr>
   <tr><th>separator Line</th></tr>
   <tr><th>CHECK</th><th>Static Text</th><th>Execute</th><th>ICON_CHECK</th><th>Check</th><th></th><th></th></tr>
   <tr><th>EXECUTE</th><th>Static Text</th><th>Execute</th><th>ICON_EXECUTE_OBJECT</th><th>Execute</th><th></th><th></th></tr>   
   <tr><th>EXEC2PC</th><th>Static Text</th><th>Execute to PC</th><th>ICON_WRITE_FILE</th><th>Execute to Local File</th><th></th><th></th></tr>
   <tr><th>EXEC2BG</th><th>Static Text</th><th>Execute as Job</th><th>ICON_SYM_SPOOL_SERVER</th><th>Execute as a Background Job</th><th></th><th></th></tr>
   <tr><th>separator Line</th></tr>   
   <tr><th>SETTING</th><th>Static Text</th><th>Settings</th><th>ICON_SETTINGS</th><th>Settings</th><th></th><th></th></tr>
   <tr><th>EXP</th><th>Static Text</th><th>COL/EXP</th><th>ICON_VIEW_EXPAND_HORIZONTAL</th><th></th><th></th><th></th></tr>  
   <tr><th>separator Line</th></tr>   
   <tr><th>MAX_ROW</th><th> </th><th> </th><th> L</th><th></th><th></th><th>W-MAX_ROW</th></tr>     
  </tbody>
 </table> 

Create Separator line by right clicking on item
![image](https://user-images.githubusercontent.com/43263062/173182795-597a78f9-4598-4f1e-841b-3a4206db5590.png)
Add Function keys as shown in image
![image](https://user-images.githubusercontent.com/43263062/173182818-d6c33953-017f-4b9b-a82c-c861bb5b0419.png)
</details>
 <details><summary><h3>PF_9100</h3></summary>
   <table>
  <thead>
   <tr><th>Function Code</th><th>Type</th><th>Function Text</th><th>Icon Name</th><th>Icon Text</th><th>Info.Text</th><th>Field Name</th></tr>
  </thead>
  <tbody>
   <tr><th>OK_QWIZ</th><th>Static Text</th><th>Close</th><th>ICON_OKAY</th><th></th><th></th><th></th></tr>  
   <tr><th>CLOSE_QWIZ</th><th>Static Text</th><th>Cancel</th><th>ICON_CANCEL</th><th></th><th></th><th></th></tr>     
  </tbody>
 </table>
  </details>
<details><summary><h3>PF_9200</h3></summary>
 <table>
  <thead>
   <tr><th>Function Code</th><th>Type</th><th>Function Text</th><th>Icon Name</th><th>Icon Text</th><th>Info.Text</th><th>Field Name</th></tr>
  </thead>
  <tbody>
   <tr><th>OK_SET</th><th>Static Text</th><th>Continue</th><th>ICON_OKAY</th><th></th><th></th><th></th></tr>  
   <tr><th>CLOSE_SET</th><th>Static Text</th><th>Close</th><th>ICON_CANCEL</th><th></th><th></th><th></th></tr>     
  </tbody>
 </table>
</details>
Create a TCODE in se93 to call ZQUERY program screen 1000 
</details>


 
